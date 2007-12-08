--[[

    Sample
        Author:    Guillaume Cartier
        Version:   1.0 - Initial release

]]


-- Version
	Version = 1.0


--
--- Sample
--


function Sample()
end


function SampleMsg(message)
	MessageBox(message);
end


--
--- Events
--


function SampleOnEvent(event)
    -- X
    if ( event == "X" ) then
        SampleMsg("X");
	
	-- Y
    elseif (event == "Y") then
        SampleMsg("Y");
        
	end
end
