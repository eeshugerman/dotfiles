hs.hotkey.bind({'cmd', 'shift'}, 'T', function()
    hs.osascript.applescript('tell app "System Events" to tell appearance preferences to set dark mode to not dark mode')
end)

-- launch spotlight with just cmd
-- wip, based on https://github.com/Hammerspoon/hammerspoon/issues/1039#issuecomment-253374355
-- local module = {}

-- module.eventwatcher1 = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
--     local flags = e:getFlags()

--     if flags.cmd and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
--       module.cmdWasPressed = true
--       module.cmdShouldBeIgnored = false
--       return false;
--     end

--     if not flags.cmd then
--       if module.cmdWasPressed and not module.cmdShouldBeIgnored then
--         hs.eventtap.keyStroke({'cmd'}, 'space')
--       end
--       module.cmdWasPressed = false
--       module.cmdShouldBeIgnored = false
--     end

--     return false;
-- end):start()

-- module.eventwatcher2 = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(e)
--     local flags = e:getFlags()
--     print('in watcher 2')
--     print('flags.cmd', flags.cmd)
--     print('module.cmdWasPressed', module.cmdWasPressed)
--     if flags.cmd and module.cmdWasPressed then
--       print('setting ignore flag...')
--       module.cmdShouldBeIgnored = true
--     end
--     return false;
-- end):start()

