FROM haskell:8.10.4

RUN apt-get update && apt-get install -y wget lsb-release software-properties-common git vim

RUN wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh && ./llvm.sh 9
ENV PATH="/usr/lib/llvm-9/bin:/usr/lib/llvm-9/lib:${PATH}" 

RUN stack install hlint 
RUN stack install stylish-haskell  
RUN stack install hindent haskell-dap ghci-dap 
