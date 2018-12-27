// Copyright 2012-2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "ppapi/cpp/audio.h"
#include "ppapi/cpp/input_event.h"
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"
#include "ppapi/cpp/var_dictionary.h"
#include "synth.hpp"

#include <methcla/platform/pepper.hpp>

#include <cmath>

class ThaddeusInstance : public pp::Instance
{
    std::unique_ptr<thaddeus::Engine> m_engine;
    pp::Size                          m_windowSize;
    bool                              m_isMouseDown;

public:
    explicit ThaddeusInstance(PP_Instance instance)
    : pp::Instance(instance)
    , m_isMouseDown(false)
    {}

    virtual ~ThaddeusInstance() {}

    virtual bool Init(uint32_t argc, const char* argn[], const char* argv[]);

    // Called by the browser to handle the postMessage() call in Javascript.
    // |var_message| is expected to be a string that contains the name of the
    // method to call.  Note that the setFrequency method takes a single
    // parameter, the frequency.  The frequency parameter is encoded as a string
    // and appended to the 'setFrequency' method name after a ':'.  Examples
    // of possible message strings are:
    //     playSound
    //     stopSound
    //     setFrequency:880
    // If |var_message| is not a recognized method name, this method does
    // nothing.
    virtual void HandleMessage(const pp::Var& var_message) override;

    virtual void DidChangeView(const pp::View& view) override;
    virtual bool HandleInputEvent(const pp::InputEvent& event) override;

private:
    std::pair<float, float> mapMousePosition(const pp::MouseInputEvent& event);

    void startVoice(const pp::MouseInputEvent& event);
    void stopVoice(const pp::MouseInputEvent& event);
    void updateVoice(const pp::MouseInputEvent& event);
};

bool ThaddeusInstance::Init(uint32_t argc, const char* argn[],
                            const char* argv[])
{
    Methcla::EngineOptions options;
    options.logHandler = [this](Methcla_LogLevel, const char* message) {
        PostMessage(pp::Var(message));
    };
    Methcla_AudioDriverOptions audioDriverOptions;
    methcla_audio_driver_options_init(&audioDriverOptions);
    Methcla_AudioDriver* driver =
        methcla_platform_pepper_audio_driver_new(&audioDriverOptions, this);
    m_engine = std::unique_ptr<thaddeus::Engine>(
        new thaddeus::Engine(options, driver));

    // try {
    //     m_engine->startVoice(0, 0.5, 0.5);
    // } catch (std::runtime_error& e) {
    //     PostMessage(pp::Var(e.what()));
    // }

    // RequestInputEvents(PP_INPUTEVENT_CLASS_MOUSE);

    return true;
}

void ThaddeusInstance::HandleMessage(const pp::Var& var_message)
{
    if (var_message.is_dictionary())
    {
        pp::VarDictionary msg(var_message);
        std::string       type(msg.Get(pp::Var("type")).AsString());
        int32_t           voiceId(msg.Get(pp::Var("id")).AsInt());
        if (type == "startVoice")
        {
            const double freq = msg.Get(pp::Var("freq")).AsDouble();
            const double amp = msg.Get(pp::Var("amp")).AsDouble();
            m_engine->startVoice(voiceId, freq, amp);
        }
        else if (type == "updateVoice")
        {
            const double freq = msg.Get(pp::Var("freq")).AsDouble();
            const double amp = msg.Get(pp::Var("amp")).AsDouble();
            m_engine->updateVoice(voiceId, freq, amp);
        }
        else if (type == "stopVoice")
        {
            m_engine->stopVoice(voiceId);
        }
    }
}

void ThaddeusInstance::DidChangeView(const pp::View& view)
{
    m_windowSize = view.GetRect().size();
}

std::pair<float, float>
ThaddeusInstance::mapMousePosition(const pp::MouseInputEvent& event)
{
    pp::Point pos = event.GetPosition();
    return std::pair<float, float>(
        m_windowSize.width() == 0.f
            ? 0.f
            : (float)pos.x() / (float)m_windowSize.width(),
        m_windowSize.height() == 0.f
            ? 0.f
            : (float)pos.y() / (float)m_windowSize.height());
}

bool ThaddeusInstance::HandleInputEvent(const pp::InputEvent& event)
{
    switch (event.GetType())
    {
        case PP_INPUTEVENT_TYPE_MOUSEDOWN:
            m_isMouseDown = true;
            startVoice(pp::MouseInputEvent(event));
            break;
        case PP_INPUTEVENT_TYPE_MOUSEUP:
            m_isMouseDown = false;
            stopVoice(pp::MouseInputEvent(event));
            break;
        case PP_INPUTEVENT_TYPE_MOUSEMOVE:
            if (m_isMouseDown)
                updateVoice(pp::MouseInputEvent(event));
            break;
        default:
            return false;
    }
    return true;
}

void ThaddeusInstance::startVoice(const pp::MouseInputEvent& event)
{
    auto pos = mapMousePosition(event);
    m_engine->startVoice(0, pos.first, pos.second);
}

void ThaddeusInstance::stopVoice(const pp::MouseInputEvent&)
{
    m_engine->stopVoice(0);
}

void ThaddeusInstance::updateVoice(const pp::MouseInputEvent& event)
{
    auto pos = mapMousePosition(event);
    m_engine->updateVoice(0, pos.first, pos.second);
}

class ThaddeusModule : public pp::Module
{
public:
    ThaddeusModule()
    : pp::Module()
    {}
    ~ThaddeusModule() {}

    virtual pp::Instance* CreateInstance(PP_Instance instance)
    {
        return new ThaddeusInstance(instance);
    }
};

namespace pp {
    Module* CreateModule() { return new ThaddeusModule(); }
} // namespace pp
