// Copyright (c) 2012 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "synth.hpp"

#include <methcla/platform/pepper.hpp>

#include "ppapi/cpp/audio.h"
#include "ppapi/cpp/input_event.h"
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"

#include <cmath>

class ThaddeusInstance : public pp::Instance
{
    std::unique_ptr<thaddeus::Engine> m_engine;
    pp::Size m_windowSize;
    bool m_isMouseDown;

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
    // If |var_message| is not a recognized method name, this method does nothing.
    virtual void HandleMessage(const pp::Var& var_message) override;

    virtual void DidChangeView(const pp::View& view) override;
    virtual bool HandleInputEvent(const pp::InputEvent& event) override;

private:
    std::pair<float,float> mapMousePosition(const pp::MouseInputEvent& event);

    void startVoice(const pp::MouseInputEvent& event);
    void stopVoice(const pp::MouseInputEvent& event);
    void updateVoice(const pp::MouseInputEvent& event);
};

bool ThaddeusInstance::Init(uint32_t argc,
                            const char* argn[],
                            const char* argv[])
{
    Methcla::EngineOptions options;
    options.logHandler = [this](Methcla_LogLevel, const char* message) {
        PostMessage(pp::Var(message));
    };
    Methcla_AudioDriverOptions audioDriverOptions;
    methcla_audio_driver_options_init(&audioDriverOptions);
    Methcla_AudioDriver* driver = methcla_platform_pepper_audio_driver_new(&audioDriverOptions, this);
    m_engine = std::unique_ptr<thaddeus::Engine>(new thaddeus::Engine(options, driver));

    // try {
    //     m_engine->startVoice(0, 0.5, 0.5);
    // } catch (std::runtime_error& e) {
    //     PostMessage(pp::Var(e.what()));
    // }

    RequestInputEvents(PP_INPUTEVENT_CLASS_MOUSE);

    return true;
}

void ThaddeusInstance::HandleMessage(const pp::Var& var_message)
{
  // if (!var_message.is_string()) {
  //   return;
  // }
  // std::string message = var_message.AsString();
  // if (message == kPlaySoundId) {
  //   audio_.StartPlayback();
  // } else if (message == kStopSoundId) {
  //   audio_.StopPlayback();
  // } else if (message.find(kSetFrequencyId) == 0) {
  //   // The argument to setFrequency is everything after the first ':'.
  //   size_t sep_pos = message.find_first_of(kMessageArgumentSeparator);
  //   if (sep_pos != std::string::npos) {
  //     std::string string_arg = message.substr(sep_pos + 1);
  //     // Got the argument value as a string: try to convert it to a number.
  //     std::istringstream stream(string_arg);
  //     double double_value;
  //     if (stream >> double_value) {
  //       SetFrequency(double_value);
  //       return;
  //     }
  //   }
  // }
}

void ThaddeusInstance::DidChangeView(const pp::View& view)
{
    m_windowSize = view.GetRect().size();
}

std::pair<float,float> ThaddeusInstance::mapMousePosition(const pp::MouseInputEvent& event)
{
    pp::Point pos = event.GetPosition();
    return std::pair<float,float>(
        m_windowSize.width() == 0.f ? 0.f : (float)pos.x()/(float)m_windowSize.width(),
        m_windowSize.height() == 0.f ? 0.f : (float)pos.y()/(float)m_windowSize.height()
        );
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
    ThaddeusModule() : pp::Module() {}
    ~ThaddeusModule() {}

    virtual pp::Instance* CreateInstance(PP_Instance instance)
    {
        return new ThaddeusInstance(instance);
    }
};

namespace pp {
    Module* CreateModule() { return new ThaddeusModule(); }
}  // namespace pp
