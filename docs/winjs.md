
  # WinJS

  ```

    winjs

      load-script: (js-filepath: string) ->
      load-library: (dll-filepath: string) ->
      load-wasm: (wasm-filepath: string) ->

      process

        args: array
        env-vars: array
        sleep: (ms: number) -> void

        io

          stdout: (string) -> void
          stderr: (string) -> void

          get-stdin: -> string

          read-ln: ->

          debug: (message: string) !->

    os

      now: -> string # yyyy-mm-dd hh:mm:ss:ms

      now-as-milliseconds: -> number

      file-system:

        file-exists: (filepath: string) -> boolean
        folder-exists: (filepath: string) -> boolean

        get-current-folder: -> string
        set-current-folder: (filepath: string) !-> void

        read-text-file: (filepath) -> string | void

  ```