/* jabotR Field Book language switcher
 * Portuguese is always the default language.
 * English is an optional secondary view.
 */
(() => {
  "use strict";

  const config = window.JabotTranslations || {};
  const strings = config.strings || {};
  const patterns = config.patterns || {};
  const messages = config.messages || {};
  const labels = config.labels || { pt: "Português", en: "English" };
  const available = config.availableLanguages || ["pt", "en"];
  const defaultLanguage = config.defaultLanguage || "pt";
  const storageKey = "jabotr-fieldbook-language";

  const textOriginals = new WeakMap();
  const attrOriginals = new WeakMap();
  const excludedSelectors = [
    "script",
    "style",
    "code",
    "pre",
    "noscript",
    "svg",
    ".jb-language-switcher"
  ].join(",");

  let currentLanguage = getInitialLanguage();
  let originalTitle = document.title;
  let observer = null;

  function normalize(value) {
    return String(value ?? "").replace(/\s+/g, " ").trim();
  }

  function getInitialLanguage() {
    const params = new URLSearchParams(window.location.search);
    const queryLanguage = params.get("lang");

    if (available.includes(queryLanguage)) {
      return queryLanguage;
    }

    try {
      const stored = localStorage.getItem(storageKey);
      if (available.includes(stored)) {
        return stored;
      }
    } catch (_error) {
      // file:// may restrict localStorage in some browsers.
    }

    return defaultLanguage;
  }

  function replaceVars(text, vars = {}) {
    let result = String(text ?? "");

    Object.entries(vars).forEach(([name, value]) => {
      result = result.replace(
        new RegExp(`\\{${name}\\}`, "g"),
        String(value)
      );
    });

    return result;
  }

  function translateValue(value, language) {
    const original = normalize(value);

    if (!original || language === defaultLanguage) {
      return original;
    }

    const dictionary = strings[language] || {};

    if (Object.prototype.hasOwnProperty.call(dictionary, original)) {
      return dictionary[original];
    }

    for (const [pattern, replacement] of (patterns[language] || [])) {
      if (pattern.test(original)) {
        pattern.lastIndex = 0;
        return original.replace(pattern, replacement);
      }
      pattern.lastIndex = 0;
    }

    return original;
  }

  function translateTextNode(node, language) {
    if (!textOriginals.has(node)) {
      textOriginals.set(node, node.nodeValue);
    }

    const original = textOriginals.get(node);
    const trimmed = normalize(original);

    if (!trimmed) return;

    const leading = original.match(/^\s*/)?.[0] || "";
    const trailing = original.match(/\s*$/)?.[0] || "";

    node.nodeValue =
      leading +
      translateValue(trimmed, language) +
      trailing;
  }

  function translateAttributes(element, language) {
    const attrs = [
      "placeholder",
      "title",
      "aria-label",
      "alt",
      "data-tip"
    ];

    attrs.forEach(attr => {
      if (!element.hasAttribute(attr)) return;

      if (!attrOriginals.has(element)) {
        attrOriginals.set(element, {});
      }

      const stored = attrOriginals.get(element);

      if (!(attr in stored)) {
        stored[attr] = element.getAttribute(attr);
      }

      element.setAttribute(
        attr,
        translateValue(stored[attr], language)
      );
    });
  }

  function translateSubtree(root, language) {
    if (!root) return;

    if (root.nodeType === Node.TEXT_NODE) {
      translateTextNode(root, language);
      return;
    }

    if (root.nodeType !== Node.ELEMENT_NODE &&
        root.nodeType !== Node.DOCUMENT_NODE) {
      return;
    }

    if (root.nodeType === Node.ELEMENT_NODE &&
        root.closest(excludedSelectors)) {
      return;
    }

    const walker = document.createTreeWalker(
      root,
      NodeFilter.SHOW_TEXT,
      {
        acceptNode(node) {
          const parent = node.parentElement;

          if (!parent || parent.closest(excludedSelectors)) {
            return NodeFilter.FILTER_REJECT;
          }

          return normalize(node.nodeValue)
            ? NodeFilter.FILTER_ACCEPT
            : NodeFilter.FILTER_REJECT;
        }
      }
    );

    const nodes = [];
    while (walker.nextNode()) {
      nodes.push(walker.currentNode);
    }

    nodes.forEach(node => translateTextNode(node, language));

    const selector =
      "[placeholder], [title], [aria-label], img[alt], [data-tip]";

    if (root.nodeType === Node.ELEMENT_NODE &&
        root.matches(selector)) {
      translateAttributes(root, language);
    }

    root.querySelectorAll?.(selector).forEach(element => {
      if (!element.closest(excludedSelectors)) {
        translateAttributes(element, language);
      }
    });
  }

  function updateTitle(language) {
    document.title = language === defaultLanguage
      ? originalTitle
      : translateValue(originalTitle, language);
  }

  function updateControl() {
    document
      .querySelectorAll(".jb-language-button")
      .forEach(button => {
        const active = button.dataset.lang === currentLanguage;

        button.classList.toggle("is-active", active);
        button.setAttribute("aria-pressed", String(active));
      });
  }

  function applyLanguage(language, persist = true) {
    if (!available.includes(language)) {
      language = defaultLanguage;
    }

    currentLanguage = language;

    document.documentElement.lang =
      language === "pt" ? "pt-BR" : "en";

    translateSubtree(document.body, language);
    updateTitle(language);
    updateControl();

    if (persist) {
      try {
        localStorage.setItem(storageKey, language);
      } catch (_error) {
        // file:// may restrict localStorage.
      }
    }

    window.dispatchEvent(
      new CustomEvent("jabot:languagechange", {
        detail: { language }
      })
    );
  }

  function createControl() {
    if (document.querySelector(".jb-language-switcher")) {
      return;
    }

    const control = document.createElement("div");
    control.className = "jb-language-switcher";
    control.setAttribute("role", "group");
    control.setAttribute("aria-label", "Idioma");

    available.forEach(language => {
      const button = document.createElement("button");

      button.type = "button";
      button.className = "jb-language-button";
      button.dataset.lang = language;
      button.textContent = labels[language] || language.toUpperCase();

      button.addEventListener("click", () => {
        applyLanguage(language);
      });

      control.appendChild(button);
    });

    document.body.appendChild(control);
    updateControl();
  }

  function observeDynamicContent() {
    if (observer) observer.disconnect();

    observer = new MutationObserver(mutations => {
      for (const mutation of mutations) {
        mutation.addedNodes.forEach(node => {
          if (node.nodeType === Node.ELEMENT_NODE ||
              node.nodeType === Node.TEXT_NODE) {
            translateSubtree(node, currentLanguage);
          }
        });
      }
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  function init() {
    createControl();
    applyLanguage(currentLanguage, false);
    observeDynamicContent();
  }

  window.JabotI18n = {
    t(key, vars = {}, fallback = "") {
      const message =
        messages[currentLanguage]?.[key] ??
        fallback;

      return replaceVars(message, vars);
    },

    getLanguage() {
      return currentLanguage;
    },

    setLanguage(language) {
      applyLanguage(language);
    },

    translate(value) {
      return translateValue(value, currentLanguage);
    }
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
