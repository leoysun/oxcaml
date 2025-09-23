FROM mcr.microsoft.com/devcontainers/universal

USER root

RUN cd /root && \
        curl -L -o autoconf.tar.gz https://ftp.gnu.org/gnu/autoconf/autoconf-2.71.tar.gz && \
        tar fxz autoconf.tar.gz && \
        cd autoconf-2.71 && \
        ./configure --prefix=/usr && \
        make && \
        make install && \
        cd .. && \
        rm -rf autoconf-2.71 autoconf.tar.gz

RUN curl -L -o /usr/bin/opam https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-i686-linux && \
        chmod +x /usr/bin/opam && \
        apt-get update && \
        apt-get remove -y swig3.0 unixodbc-dev libpq-dev \
        default-libmysqlclient-dev libgdiplus jq python3-pip sqlite3 \
        libsqlite3-dev tk-dev uuid-dev clang lldb llvm gdb vim-doc xtail \
        libsecret-1-dev && \
        apt-get clean && \
        rm -rf /var/lib/apt/lists/*

RUN opam init -a --disable-sandboxing --yes --bare && \
        opam update -a && \
        opam switch create 4.14.0 --yes  && \
        eval $(opam env --switch 4.14.0) && \
        opam install --yes  ocamlformat merlin async ocaml-lsp-server utop core_unix \
                async async_extra async_js async_kernel async_rpc_kernel async_rpc_websocket \
                base core core_kernel ocaml-embed-file \
                ppx_jane virtual_dom cohttp cohttp-async uri \
                bonsai js_of_ocaml js_of_ocaml-ppx

