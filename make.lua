#!/usr/bin/env lua

--------------
-- Settings --
--------------

-- Set this to true to use a better compression algorithm for the DAC driver.
-- Having this set to false will use an inferior compression algorithm that
-- results in an accurate ROM being produced.
local improved_dac_driver_compression = false

---------------------
-- End of settings --
---------------------

---------------
-- Utilities --
---------------

local function amend_sound_driver_size()
	-- Correct the compressed sound driver size, which we couldn't do until p2bin had been ran.
	local comp_z80_size, movewZ80CompSize

	for line in io.lines("main.h") do
		local match_begin, match_end = string.find(line, "comp_z80_size")

		if match_begin ~= nil then
			comp_z80_size = tonumber(line:match("0x%x+", match_end))
		end

		local match_begin, match_end = string.find(line, "movewZ80CompSize")

		if match_begin ~= nil then
			movewZ80CompSize = tonumber(line:match("0x%x+", match_end))
		end
	end

	if comp_z80_size ~= nil and movewZ80CompSize ~= nil then
		local rom = io.open("s2built.bin", "r+b")

		rom:seek("set", movewZ80CompSize + 2)
		rom:write(string.pack(">I2", comp_z80_size))

		rom:close()
	end

	-- Remove the header file, since we no longer need it.
	os.remove("main.h")
end

----------------------
-- End of utilities --
----------------------

-------------------------------------
-- Actual build script begins here --
-------------------------------------

local common = require "tools.lua.common"

-- Build the ROM.
local compression = improved_dac_driver_compression and "saxman-optimised" or "saxman-bugged"
common.build_rom_and_handle_failure("main", "s2built", "", "-p=0 -z=0," .. compression .. ",Size_of_Snd_driver_guess,after", true, "https://github.com/sonicretro/s2disasm")

-- Patch the ROM with the correct sound driver size.
amend_sound_driver_size()

-- A successful build; we can quit now.
common.exit()
