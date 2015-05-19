local lil = require'lilikoi'
local pp = require'pp'

pp(lil.translate"( 7 / 2 )")
pp(lil.run"( 7 / 2 )")
pp(lil.translate"2.5 * 4 - ( 7 / 2 )")
pp(lil.run"2.5 * 4 - ( 7 / 2 )")
pp(lil.translate"1 + 2 * 3 / ( 10 * 1.2 )")
pp(lil.run"1 + 2 * 3 / ( 10 * 1.2 )")
pp(lil.translate"1 + 2 * ( 7 + 8 - 3 ) / ( 10 * ( 6 / 5 ) )")
pp(lil.run"1 + 2 * ( 7 + 8 - 3 ) / ( 10 * ( 6 / 5 ) )")
pp(lil.translate"[ 10 20 30 ]")
pp(lil.run"[ 10 20 30 ]")
pp(lil.translate"1 + math.pi")
pp(lil.run"1 + math.pi")
pp(lil.translate"( 2 math.pow 3 )")
pp(lil.run"( 2 math.pow 3 )")

pp'OK'
