hs.hotkey.bind({"cmd", "shift"}, "T", function()
    hs.osascript.applescript('tell app "System Events" to tell appearance preferences to set dark mode to not dark mode')
end)
