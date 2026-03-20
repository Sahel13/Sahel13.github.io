(() => {
  const root = document.documentElement;
  const button = document.querySelector(".theme-toggle");
  const storageKey = "theme-preference";
  const media = window.matchMedia("(prefers-color-scheme: dark)");

  if (!button) {
    return;
  }

  const readStoredTheme = () => {
    try {
      const storedTheme = localStorage.getItem(storageKey);
      return storedTheme === "light" || storedTheme === "dark" ? storedTheme : null;
    } catch (_) {
      return null;
    }
  };

  const preferredTheme = () => readStoredTheme() || (media.matches ? "dark" : "light");

  const applyTheme = (theme) => {
    const isDark = theme === "dark";
    root.dataset.theme = theme;
    button.textContent = isDark ? "Light mode" : "Dark mode";
    button.setAttribute("aria-pressed", isDark ? "true" : "false");
    button.setAttribute("aria-label", isDark ? "Switch to light mode" : "Switch to dark mode");
  };

  const handleSystemChange = (event) => {
    if (readStoredTheme() === null) {
      applyTheme(event.matches ? "dark" : "light");
    }
  };

  applyTheme(preferredTheme());

  button.addEventListener("click", () => {
    const nextTheme = root.dataset.theme === "dark" ? "light" : "dark";
    try {
      localStorage.setItem(storageKey, nextTheme);
    } catch (_) {}
    applyTheme(nextTheme);
  });

  if (typeof media.addEventListener === "function") {
    media.addEventListener("change", handleSystemChange);
  } else if (typeof media.addListener === "function") {
    media.addListener(handleSystemChange);
  }
})();
