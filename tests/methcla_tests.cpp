// Copyright (C) 2012-2013 Samplecount S.L.
// Copyright (C) 2026 Methcla contributors
//
// SPDX-License-Identifier: Apache-2.0

#include "methcla_tests.hpp"

static std::string gInputFileDirectory = "tests/input";
static std::string gOutputFileDirectory = "tests/output";

void Methcla::Tests::initialize(std::string inputFileDirectory,
                                std::string outputFileDirectory)
{
    gInputFileDirectory = inputFileDirectory;
    gOutputFileDirectory = outputFileDirectory;
}

std::string Methcla::Tests::inputFile(const std::string& name)
{
    return gInputFileDirectory + "/" + name;
}

std::string Methcla::Tests::outputFile(const std::string& name)
{
    return gOutputFileDirectory + "/" + name;
}

int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
