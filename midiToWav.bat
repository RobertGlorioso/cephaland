for /F "tokens=*" %%A in (tmp.txt) do (timidity -c "./soundfonts/GeneralUser GS v1.471.cfg.txt" -Ow %%A -o ./resource/sfx/%%~nA.wav)