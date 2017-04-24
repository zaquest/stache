// Currently runtime consists of only one function
define([], function () {
    return function (html) {
        var p = document.createElement('p');
        p.appendChild(document.createTextNode(str));
        return p.innerHTML;
    };
});
