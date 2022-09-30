hs.hotkey.bind({'cmd', 'shift'}, 'T', function()
    hs.osascript.applescript('tell app "System Events" to tell appearance preferences to set dark mode to not dark mode')
end)

hs.hotkey.bind({'cmd', 'shift'}, 'H', function()
    hs.reload()
end)

-- launch spotlight with just cmd
-- based on https://github.com/Hammerspoon/hammerspoon/issues/1039#issuecomment-253374355
local spolightCmdModule = {}

spolightCmdModule.eventwatcher1 = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
    local flags = e:getFlags()

    if flags.cmd and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
      spolightCmdModule.cmdWasPressed = true
      spolightCmdModule.cmdShouldBeIgnored = false
      return false;
    end

    if not flags.cmd then
      if spolightCmdModule.cmdWasPressed and not spolightCmdModule.cmdShouldBeIgnored then
        hs.eventtap.keyStroke({'cmd'}, 'space')
      end
      spolightCmdModule.cmdWasPressed = false
      spolightCmdModule.cmdShouldBeIgnored = false
    end

    return false;
end):start()

spolightCmdModule.eventwatcher2 = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(e)
    local flags = e:getFlags()
    if flags.cmd and spolightCmdModule.cmdWasPressed then
      spolightCmdModule.cmdShouldBeIgnored = true
    end
    return false;
end):start()

