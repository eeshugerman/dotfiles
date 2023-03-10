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
-- NOTE: if spotlight pops up when it shouldn't, try a reboot (and OS update?)
local SpotlightCmd = {
  cmdIsPressed = false,
  somethingElseHappened = false,
  reset = function(self)
    self.cmdIsPressed = false
    self.somethingElseHappened = false
  end
}

eventwatcher1 = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(e)
    local flags = e:getFlags()
    local anyOtherFlag = flags.alt or flags.shift or flags.ctrl or flags.fn

    if flags.cmd and not anyOtherFlag then
      SpotlightCmd.cmdIsPressed = true
    end

    if SpotlightCmd.cmdIsPressed and anyOtherFlag then
      SpotlightCmd.somethingElseHappened = true
    end

    if not flags.cmd and not anyOtherFlag and SpotlightCmd.cmdIsPressed and not SpotlightCmd.somethingElseHappened then
      hs.eventtap.keyStroke({'cmd'}, 'space')
    end

    if not flags.cmd then
      SpotlightCmd:reset()
    end
end):start()


eventwatcher2 = hs.eventtap.new({'all', hs.eventtap.event.types.flagsChanged}, function(e)
    if SpotlightCmd.cmdIsPressed then
      SpotlightCmd.somethingElseHappened = true
    end
end):start()

