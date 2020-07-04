if exists("g:loaded_viminspect") || &cp || v:version < 700
  let g:loaded_repeat = 1
  finish
else
  let s:loaded = 1
  let s:connected = 0
  let s:term = 0
  let s:old_term = 0
  let s:window = 0
  let s:error_message = 0
endif

let s:bits = has('num64') ? 64 : 32
let s:mask = s:bits - 1
let s:mask32 = 32 - 1

let s:pow2 = [1]
for s:i in range(s:mask)
  call add(s:pow2, s:pow2[-1] * 2)
endfor
unlet s:i

let s:min = s:pow2[-1]

func! s:lshift(a, n) abort
  return a:a * s:pow2[and(a:n, s:mask)]
endfunc

func! s:rshift(a, n) abort
  let n = and(a:n, s:mask)
  return n == 0 ? a:a :
  \  a:a < 0 ? (a:a - s:min) / s:pow2[n] + s:pow2[-2] / s:pow2[n - 1]
  \          : a:a / s:pow2[n]
endfunc

" 32bit or 64bit specific method
" define sign_extension
"        lshift32/rshift32 64bit only implementation.
if has('num64')
  " NOTE:
  " An int literal larger than or equal to 0x8000000000000000 will be rounded
  " to 0x7FFFFFFFFFFFFFFF after Vim 8.0.0219, so create it without literal
  let s:xFFFFFFFF00000000 = 0xFFFFFFFF * s:pow2[and(32, s:mask)]
  func! s:sign_extension(n) abort
    if and(a:n, 0x80000000)
      return or(a:n, s:xFFFFFFFF00000000)
    else
      return and(a:n, 0xFFFFFFFF)
    endif
  endfunc
  func! s:lshift32(a, n) abort
    return and(s:lshift(a:a, and(a:n, s:mask32)), 0xFFFFFFFF)
  endfunc
  func! s:rshift32(a, n) abort
    return s:rshift(and(a:a, 0xFFFFFFFF), and(a:n, s:mask32))
  endfunc
else
  func! s:sign_extension(n) abort
    return a:n
  endfunc

  let s:lshift32 = s:lshift
  let s:rshift32 = s:rshift
endif

let s:Generator = {}

func! s:Generator.next() abort
  let t = xor(self._x, s:lshift(self._x, 11))
  let w = self._w
  let self._x = self._y
  let self._y = self._z
  let self._z = self._w
  let self._w = xor(xor(w, s:rshift32(w, 19)), xor(t, s:rshift32(t, 8)))
  return s:sign_extension(self._w)
endfunc

" 0x80000000 in 32bit and 0xFFFFFFFF80000000 in 64bit
func! s:Generator.min() abort
  return -2147483648
endfunc

" 0x7FFFFFFF in 32bit/64bit
func! s:Generator.max() abort
  return 2147483647
endfunc

func! s:_fmix32(x) abort
  let x = and(0xFFFFFFFF, a:x)
  let x = and(0xFFFFFFFF, 0x85EBCA6B * xor(x, s:rshift(x, 16)))
  let x = and(0xFFFFFFFF, 0xC2B2AE35 * xor(x, s:rshift(x, 13)))
  return xor(x, s:rshift(x, 16))
endfunc

func! s:Generator.seed(seeds) abort
  let x = 123456789
  for seed in a:seeds
    let x = s:_fmix32(x + seed)
  endfor

  let s = [0, 0, 0, 0]
  for i in range(4)
    let x += 0x9E3779B9
    let s[i] = s:_fmix32(x)
  endfor
  let [self._x, self._y, self._z, self._w] = s
endfunc

func! s:new_generator() abort
  let gen = deepcopy(s:Generator)
  call gen.seed([])
  return gen
endfunc


func! s:_common_generator() abort
  if !exists('s:common_generator')
    let s:common_generator = s:new_generator()
  endif
  return s:common_generator
endfunc

func! s:srand(...) abort
  if a:0 == 0
    let x = has('reltime') ? reltime()[1] : localtime()
  elseif a:0 == 1
    let x = a:1
  else
    throw 'vital: Random.Xor128: srand(): too many arguments'
  endif
  call s:_common_generator().seed([x])
endfunc

func! s:rand() abort
  return s:_common_generator().next()
endfunc

call s:srand()

let s:rfc4648_encode_table = [
      \ 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
      \ 'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
      \ 'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
      \ 'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/']

func! s:b64encode(bytes) abort
  let table = s:rfc4648_encode_table
  let pad = '='

  let b64 = []
  for i in range(0, len(a:bytes) - 1, 3)
    let n = a:bytes[i] * 0x10000
          \ + get(a:bytes, i + 1, 0) * 0x100
          \ + get(a:bytes, i + 2, 0)
    call add(b64, table[n / 0x40000])
    call add(b64, table[n / 0x1000 % 0x40])
    call add(b64, table[n / 0x40 % 0x40])
    call add(b64, table[n % 0x40])
  endfor
  if len(a:bytes) % 3 == 1
    let b64[-1] = pad
    let b64[-2] = pad
  endif
  if len(a:bytes) % 3 == 2
    let b64[-1] = pad
  endif
  return join(b64, '')
endfunc

func! s:generate_secret(num) abort
  let secret = []

  while len(secret) < a:num
    let secret = add(secret, abs(s:rand()) % 255)
  endwhile

  echo s:b64encode(secret)
endfunc

let s:CRLF = "\r\n"

func! s:NewWebsocket() abort
  let self = {}
  let self._interval = 200
  let self.connected = 0
  return self
endfunc

func! s:on_websocket(self, message, on_connected) abort
  let self = a:self

  if self.connected == 0
    let l:data = split(a:message, s:CRLF, 1)
    let connected = 0
    for line in data
      echom line
      if connected == 0
        if line == "HTTP/1.1 101 Switching Protocols"
          let connected = 1
        endif
      else
        if line == ""
          let empties = empties + 1
        else
          let empties = 0
        endif
      endif
    endfor

    if connected == 1 && empties == 2
      let self.connected = 1
      call a:on_connected(self.channel)
    endif
  else
    echom a:message
  end
endfunc

func! s:connect(ws, address, path, on_connected) abort
  let self = a:ws

  let self.secret = s:generate_secret(16)

  let self.channel = ch_open(a:address, { 'mode': 'raw', 'callback': {_, msg -> s:on_websocket(self, msg, a:on_connected)} })

  if ch_status(self.channel) == "open"
    let req = 'GET ' . a:path . ' HTTP/1.1' . s:CRLF .
      \ 'Host: ' . a:address . s:CRLF .
      \ 'Upgrade: websocket' . s:CRLF .
      \ 'Connection: Upgrade' . s:CRLF .
      \ 'Sec-WebSocket-Key: ' . self.secret . s:CRLF .
      \ 'Sec-WebSocket-Version: 13' . s:CRLF .
      \ s:CRLF

    call ch_sendraw(self.channel, req)
  endif

  " Start timer
  "let timer = timer_start(self._interval, {-> ch_status(handler)}, {'repeat': -1})
  "
endfunc

func! s:write_frame(channel, data) abort
  " 1 (final frame), 0 0 0 (rsv), 1 (text mode), 1 (mask)
  if len(a:data) <= 125
    let l:header = 0z81 + s:u8tobyte(or(0xF0, len(a:data))) + 0z0000
  else
    let l:header = 0z81FE + s:u16tobyte(len(a:data)) + 0z0000
  endif
  echom a:data
  call ch_sendraw(a:channel, l:header)
  call ch_sendraw(a:channel, a:data)
endfunc

let s:error=''
func! s:error(msg) abort
  if &ut!=11|let s:hold_ut=&ut|let &ut=11|en
  let s:error=a:msg
  aug Pecho
    au CursorHold * if s:error!=''|echohl WarningMsg|echo s:error|echohl None
          \|let s:error=''|let &ut=s:hold_ut|en
        \|aug s:error|exe 'au!'|aug END|aug! s:error
  aug END
endfunc

func! s:show_buffer(id) abort
  if a:id <= 0
    return
  endif

  let l:window = win_getid()
  let l:winview = winsaveview()

  if s:window > 0 && win_gettype(s:window) != "unknown"
    call win_gotoid(s:window)
  else
    10split
    let s:window = win_getid()
  endif

  execute("buffer " . a:id)

  call win_gotoid(l:window)
  call winrestview(l:winview)
endfunc

func! s:close_window() abort
  if s:window <= 0
    return
  endif

  let l:window = win_getid()
  let l:winview = winsaveview()
  call win_gotoid(s:window)
  quit
  let s:window = 0
  call win_gotoid(l:window)
  call winrestview(l:winview)
endfunc

func! s:on_connected(channel) abort
  let s:connected = 1
  echom "Debugging process started successfully!"
  call s:show_buffer(s:term)
  call s:write_frame(a:channel, '{"id": 1, "method": "Runtime.enable"}')
  call s:write_frame(a:channel, '{"id": 2, "method": "Runtime.runIfWaitingForDebugger"}')
  " call s:write_frame(a:channel, '{"id": 2, "method": "Debugger.enable"}')
  " call s:write_frame(a:channel, '{"id": 4, "method": "Debugger.resume"}')
endfunc

let g:connected = 0

func! s:on_output(channel, output) abort
  let l:data = split(a:output, '\r\?\n', 1)

  for l:line in l:data
    if l:line =~ "^Debugger listening on " && g:connected == 0
      let g:connected = 1
      let [address, token] = split(l:line[27:], "/")

      let ws = s:NewWebsocket()
      call s:connect(ws, address, "/" . token, function('s:on_connected'))
      return
    end
  endfor
endfunc

func! s:on_close(channel) abort
  let job_info = job_info(ch_getjob(a:channel))
  if s:connected == 0
    call s:show_buffer(s:term)
  endif
endfunc

func! viminspect#Inspect(command) abort

  if s:term > 0
    if bufexists(s:term)
      execute("bw! " . s:term)
    endif
  endif

  if v:version < 800
    echoerr 'NodeInspect requires VIM version 8 or higher'
    return
  endif

  let s:term = term_start(a:command, {
        \ "in_io": "null",
        \ "stoponexit": "kill",
        \ "env": { "NODE_OPTIONS": "--inspect-brk=127.0.0.1:0" },
        \ "close_cb": function("s:on_close"),
        \ "callback": function("s:on_output"),
        \ "hidden": 1,
        \ "norestore": 1,
        \ "term_kill": "kill"
        \ })
endfunc

func! s:to_byte(bytes) abort
  return eval('0z' . join(map(copy(a:bytes), 'printf(''%02x'', v:val)'), ''))
endfunc

func! s:u16tobyte(num) abort
  return s:to_byte([and(s:rshift(a:num, 8), 0xFF), and(a:num, 0xFF)])
endfunc

func! s:u8tobyte(num) abort
  return s:to_byte([and(a:num, 0xFF)])
endfunc

func! viminspect#Test(text) abort
  return s:frame_header(a:text)
endfunc
