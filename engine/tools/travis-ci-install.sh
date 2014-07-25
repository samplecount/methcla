sudo apt-get install --allow-unauthenticated -qq clang-3.4
# export CXXFLAGS="-std=c++0x -stdlib=libc++"
svn co --quiet http://llvm.org/svn/llvm-project/libcxx/trunk libcxx
cd libcxx/lib && bash buildit
sudo cp ./libc++.so.1.0 /usr/lib/
sudo mkdir /usr/include/c++/v1
cd .. && sudo cp -r include/* /usr/include/c++/v1/
cd /usr/lib && sudo ln -sf libc++.so.1.0 libc++.so
sudo ln -sf libc++.so.1.0 libc++.so.1
cd $cwd

