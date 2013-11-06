// c++ -std=c++11 -stdlib=libc++ -I include -o build/dumposcfile tools/dumposcfile.cpp 

#include <iostream>
#include <oscpp/detail/host.hpp>
#include <oscpp/print.hpp>
#include <oscpp/server.hpp>
#include <sstream>
#include <vector>

int main(int argc, const char* const* argv)
{
    try
    {
        if (argc < 2)
        {
            throw std::runtime_error("Usage: dumposcfile FILE");
        }

        FILE* file = fopen(argv[1], "rb");
        if (file == nullptr)
        {
            std::stringstream s;
            s << "Couldn't open file " << argv[1];
            throw std::runtime_error(s.str());
        }
        std::shared_ptr<FILE> filePtr(file, fclose);

        std::vector<char> buffer(8192, 0);

        while (true)
        {
            int32_t size;
            size_t n = fread(&size, sizeof(size), 1, file);
            if (n != 1) break;
            size = OSCPP::convert32<OSCPP::NetworkByteOrder>(size);
            if (buffer.size() < size)
                buffer.resize(size);
            n = fread(buffer.data(), 1, size, file);
            if (n != size)
                throw std::runtime_error("Couldn't read packet");
            OSCPP::Server::Packet packet(buffer.data(), size);
            std::cout << packet << std::endl;
        }
    }
    catch (std::exception& e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }

    return 0;
}
