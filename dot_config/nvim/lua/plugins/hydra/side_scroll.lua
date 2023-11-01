return require("hydra")({
  name = "Side scroll",
  mode = "n",
  body = "z",
  heads = {
    { "h", "zh" },
    { "l", "zl", { desc = "←/→" } },
    { "H", "zH" },
    { "L", "zL", { desc = "half screen ←/→" } },
  },
})
