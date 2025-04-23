# --- build stage ---
FROM erlang:26-alpine AS build
WORKDIR /app

COPY rebar.config rebar.lock ./
RUN apk add --no-cache git && rebar3 get-deps

COPY ./src ./src

RUN rebar3 clean
RUN rebar3 compile
RUN rebar3 release

# --- run stage ---
FROM alpine:3.21
WORKDIR /app

RUN apk add --no-cache bash openssl busybox libstdc++ libgcc

COPY --from=build /app/_build/default/rel/sudoku_solver ./

EXPOSE 4000
CMD /bin/sh -c "./bin/sudoku_solver foreground"
