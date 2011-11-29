## Memory allocation

We're looking for a fast user-space storage allocator.

Here are some options:

Link                License         Notes
----                -------         -----
[RTMalloc][]        GPL/LGPL        Two-level segregated fit, high performance
[tlsf][]            PD              Another TLSF implementation
[BGET][]            Public Domain   First fit or best fit, performance?
[dlmalloc][]        PD              Best fit, not threadsafe, realtime safe?
[nedmalloc][]       Boost           dlmalloc derivate, multithreaded, fast, realtime safe?
[ptmalloc3][]       PD?             dlmalloc derivate, realtime safe?
[tcmalloc][]        ?               Includes memory profiler, realtime safe?
[BSA++][]           GPL             Adaptive allocator

[BGET]:         http://www.fourmilab.ch/bget/
[RTMalloc]:     http://rtportal.upv.es/rtmalloc/
[dlmalloc]:     http://gee.cs.oswego.edu/dl/html/malloc.html
[nedmalloc]:    http://www.nedprod.com/programs/portable/nedmalloc/index.html
[ptmalloc3]:    http://www.malloc.de/
[tcmalloc]:     http://code.google.com/p/google-perftools/
[tlsf]:         http://tlsf.baisoku.org/
[BSA++]:        http://www.ercoppa.org/malloc/bsapp.htm

### Literature

See [IBM Inside memory management](http://www.ibm.com/developerworks/linux/library/l-memory/) for an overview and also the seminal paper [Dynamic Storage Allocation: A Survey and Critical Review][Wilson95] by Wilson et. al.

[Wilson95]:     ftp://osinside.net/pub/DynamicStorageAllocationSurvey-.pdf
