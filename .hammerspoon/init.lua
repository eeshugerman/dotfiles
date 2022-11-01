hs.hotkey.bind({'cmd', 'shift'}, 'T', function()
    hs.osascript.applescript('tell app "System Events" to tell appearance preferences to set dark mode to not dark mode')
end)

hs.hotkey.bind({'cmd', 'shift'}, 'H', function()
    hs.reload()
    -- neither of these are working?
    hs.notify.new({title='Hammerspoon', informativeText='Config reloaded'}):send()
    hs.alert.show('Hammerspoon config reloaded')
end)


-- doesn't work :(
-- hs.hotkey.bind({}, 'capslock', function()
--     hs.eventtap.keyStroke('escape')
-- end)

-- works, but catches capslock too because of macos prefs setting
-- hs.hotkey.bind({}, 'escape', function()
--     hs.hid.capslock.toggle()
-- end)


-- launch spotlight with just cmd
-- based on https://github.com/Hammerspoon/hammerspoon/issues/1039#issuecomment-253374355
local spotlightCmdModule = {
  cmdOnlyIsPressed = false,
  otherModifierWasPressed = false
}

spotlightCmdModule.eventwatcher1 = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
    local flags = e:getFlags()

    if flags.cmd and (flags.alt or flags.shift or flags.ctrl or flags.fn) then
      spotlightCmdModule.otherModifierWasPressed = true
      return false;
    end

    if flags.cmd and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
      spotlightCmdModule.cmdOnlyIsPressed = true
      return false;
    end

    if not flags.cmd and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
      if spotlightCmdModule.cmdOnlyIsPressed and not spotlightCmdModule.otherModifierWasPressed then
        hs.eventtap.keyStroke({'cmd'}, 'space')
      end
      spotlightCmdModule.cmdOnlyIsPressed = false
      spotlightCmdModule.otherModifierWasPressed = false
      return false;
    end

end):start()

spotlightCmdModule.eventwatcher2 = hs.eventtap.new({'all', hs.eventtap.event.types.flagsChanged}, function(e)
    if spotlightCmdModule.cmdOnlyIsPressed then
      spotlightCmdModule.cmdOnlyIsPressed = false
    end
    return false;
end):start()

