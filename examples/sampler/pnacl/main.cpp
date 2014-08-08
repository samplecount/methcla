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

#include "Engine.hpp"

#include <methcla/platform/pepper.hpp>
#include <methcla/plugins/soundfile_api_libsndfile.h>

#include "ppapi/cpp/audio.h"
#include "ppapi/cpp/input_event.h"
#include "ppapi/cpp/instance.h"
#include "ppapi/cpp/module.h"
#include "ppapi/cpp/var.h"
#include "ppapi/cpp/var_dictionary.h"

#include <nacl_io/nacl_io.h>
#include <stdio.h>
#include <sys/mount.h>

#include <cmath>
#include <memory>
#include <sstream>
#include <thread>
#include <unordered_map>

namespace Methcla { namespace Examples { namespace Sampler {

class SamplerInstance : public pp::Instance
{
    std::unique_ptr<Engine> m_engine;

public:
    explicit SamplerInstance(PP_Instance instance)
        : pp::Instance(instance)
    {}

    virtual ~SamplerInstance() {}

    virtual bool Init(uint32_t argc, const char* argn[], const char* argv[]) override;
    virtual void HandleMessage(const pp::Var& var_message) override;
};

class SamplerModule : public pp::Module
{
public:
    SamplerModule() : pp::Module() {}
    ~SamplerModule() {}

    virtual pp::Instance* CreateInstance(PP_Instance instance)
    {
        return new SamplerInstance(instance);
    }
};

} } }

using namespace Methcla::Examples::Sampler;

static std::unordered_map<std::string,std::string> argumentMap(
    uint32_t argc,
    const char* argn[],
    const char* argv[])
{
    std::unordered_map<std::string,std::string> result;
    for (uint32_t i=0; i < argc; i++)
        result[argn[i]] = argv[i];
    return result;
}

bool SamplerInstance::Init(uint32_t argc,
                           const char* argn[],
                           const char* argv[])
{
    nacl_io_init_ppapi(
        pp_instance(),
        pp::Module::Get()->get_browser_interface()
    );

    // umount("/");
    // mount("", "/", "memfs", 0, "");
    mount(
        "http://localhost:3000/sampler",
        "/http",
        "httpfs",
        0,
        ""
    );

    mount("",                                       // source
          "/sounds",                                // target
          "html5fs",                                // filesystemtype
          0,                                        // mountflags
          "type=PERSISTENT,expected_size=3268608");  // data

    return true;
}

static size_t copyFile(const char* inputFileName, const char* outputFileName)
{
    auto inputFile = std::shared_ptr<FILE>(fopen(inputFileName, "rb"), fclose);
    auto outputFile = std::shared_ptr<FILE>(fopen(outputFileName, "wb"), fclose);

    size_t numBytesRead = 0;

    if (inputFile == nullptr)
    {
        std::stringstream s;
        s << "Couldn't open input file " << inputFileName;
        throw std::runtime_error(s.str());
    }
    else if (outputFile == nullptr)
    {
        std::stringstream s;
        s << "Couldn't open output file " << outputFileName;
        throw std::runtime_error(s.str());
    }
    else
    {
        std::vector<char> buffer(8192);

        while (true)
        {
            const size_t nr = fread(buffer.data(), 1, buffer.size(), inputFile.get());
            if (nr == 0)
            {
                if (feof(inputFile.get()))
                {
                    break;
                }
                else
                {
                    throw std::runtime_error(strerror(errno));
                }
            }

            const size_t nw = fwrite(buffer.data(), 1, nr, outputFile.get());
            if (nw != nr)
            {
                if (ferror(outputFile.get()))
                {
                    throw std::runtime_error(strerror(errno));
                }
                else
                {
                    throw std::runtime_error("Short byte count while copying");
                }
            }

            numBytesRead += nr;
        }
    }

    return numBytesRead;
}

static size_t readFile(const std::string& inputFileName)
{
    auto inputFile = std::shared_ptr<FILE>(fopen(inputFileName.c_str(), "rb"), fclose);

    size_t numBytesRead = 0;

    if (inputFile == nullptr)
    {
        std::stringstream s;
        s << "Couldn't open input file " << inputFileName;
        throw std::runtime_error(s.str());
    }
    else
    {
        std::vector<char> buffer(8192);

        while (true)
        {
            const size_t nr = fread(buffer.data(), 1, buffer.size(), inputFile.get());
            if (nr == 0)
            {
                if (feof(inputFile.get()))
                {
                    break;
                }
                else
                {
                    throw std::runtime_error(strerror(errno));
                }
            }

            numBytesRead += nr;
        }
    }

    return numBytesRead;
}


void SamplerInstance::HandleMessage(const pp::Var& var_message)
{
    if (var_message.is_dictionary())
    {
        pp::VarDictionary msg(var_message);
        std::string type(msg.Get(pp::Var("type")).AsString());
        if (type == "copyFiles")
        {
            std::thread([this,type]{
                for (auto sound : { "157965", "45394", "209331" })
                {
                    const std::string inputFileName(std::string("/http/sounds/") + sound);
                    const std::string outputFileName(std::string("/sounds/") + sound);

                    try
                    {
                        size_t numBytes = copyFile(inputFileName.c_str(), outputFileName.c_str());
                        std::stringstream s;
                        s << "Copied " << numBytes
                          << " bytes from " << inputFileName
                          << " to " << outputFileName;
                        PostMessage(s.str());
                    }
                    catch (std::exception& e)
                    {
                        PostMessage(e.what());
                    }
                }

                pp::VarDictionary response;
                response.Set("type", type);
                PostMessage(response);
            }).detach();
        }
        else if (type == "createEngine")
        {
            try {
                std::thread([this,type]{
                    Engine::Options options;
                    options.engineOptions.logHandler = [this](Methcla_LogLevel, const char* message) {
                        PostMessage(pp::Var(message));
                    };
                    options.engineOptions.logFlags = kMethcla_EngineLogDebug;
                    options.engineOptions.addLibrary(methcla_soundfile_api_libsndfile);
                    // options.soundDir = "/sounds/";
                    options.sounds = { "/sounds/157965", "/sounds/45394", "/sounds/209331" };

                    Methcla_AudioDriverOptions audioDriverOptions;
                    methcla_audio_driver_options_init(&audioDriverOptions);
                    options.audioDriver = methcla_platform_pepper_audio_driver_new(&audioDriverOptions, this);
                    
                    m_engine = std::unique_ptr<Engine>(new Engine(options));

                    pp::VarDictionary response;
                    response.Set("type", type);
                    PostMessage(response);
                }).detach();
            }
            catch (std::exception& e)
            {
                pp::VarDictionary response;
                response.Set("type", type);
                response.Set("error", true);
                response.Set("error_message", e.what());
                PostMessage(response);
            }
        }
        else if (m_engine != nullptr)
        {
            if (type == "useDisk")
            {
                m_engine->useDisk(msg.Get(pp::Var("flag")).AsBool());
            }
            else
            {
                int32_t voiceId(msg.Get(pp::Var("id")).AsInt());
                if (type == "startVoice")
                {
                    const int32_t sound = msg.Get(pp::Var("sound")).AsInt();
                    const double amp    = msg.Get(pp::Var("amp")).AsDouble();
                    const double rate   = msg.Get(pp::Var("rate")).AsDouble();
                    m_engine->startVoice(voiceId, sound, amp, rate);
                }
                else if (type == "updateVoice")
                {
                    const double amp  = msg.Get(pp::Var("amp")).AsDouble();
                    const double rate = msg.Get(pp::Var("rate")).AsDouble();
                    m_engine->updateVoice(voiceId, amp, rate);
                }
                else if (type == "stopVoice")
                {
                    m_engine->stopVoice(voiceId);
                }
            }
        }
    }
}

namespace pp {
    Module* CreateModule() { return new SamplerModule(); }
}  // namespace pp
