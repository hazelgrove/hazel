// This file should contain plain JavaScript code that can run in the browser.
// It exports a function that takes a `selector` and extracts text and mapping.

module.exports = function(selector) {
    var parent = document.querySelector(selector);
    if (!parent) {
        throw new Error("Parent not found");
    }

    var textContent = "";
    var mapping = [];

    function getCssPath(el) {
        var path = [];
        var current = el;
        while (current && current !== parent) {
            var selector = current.tagName.toLowerCase();
            if (current.id) {
                selector += "#" + current.id;
            } else {
                if (current.className) {
                    selector += "." + current.className.trim().replace(/\s+/g, ".");
                }
                var siblings = current.parentNode ? Array.from(current.parentNode.children) : [];
                var index = siblings.indexOf(current);
                selector += ":nth-child(" + (index + 1) + ")";
            }
            path.unshift(selector);
            current = current.parentElement;
        }
        return path.join(" > ");
    }

    function traverse(element) {
        var childNodes = element.childNodes;
        for (var i = 0; i < childNodes.length; i++) {
            var child = childNodes[i];
            if (child.nodeType === Node.TEXT_NODE && child.textContent) {
                var baseIndex = textContent.length;
                textContent += child.textContent;
                var selectorPath = getCssPath(element);
                for (var j = 0; j < child.textContent.length; j++) {
                    mapping.push({ index: baseIndex + j, selectorPath: selectorPath });
                }
            } else if (child.nodeType === Node.ELEMENT_NODE) {
                traverse(child);
            }
        }
    }

    traverse(parent);

    return { text: textContent, mapping: mapping };
};
