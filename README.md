# stache

Escaped value

```
{{value.path}}
```

Unescaped value

```
{{{value.path}}}
```

`if-else` expressions

```
{{#if value.path}}
  first
{{#elif value.path}}
  second
{{#else}}
  other
{{/if}}
```

Loop

```
<ul>
{{#each values.path}}
  <li>{{firstname}} {{lastname}}</li>
{{/each}}
</ul>
```

Note:

  * No expressions can be computed in `if`'s test block, or anywhere
  else.
  * Inside `each` block outer context is inaccessible. If you have an
  object like `{key1: 1, items: [...]}` then
  ```
  {{#each items}}
    you can't access `key1` from here.
  {{\each}}
  ```

## Usage

```
stachec js path/to/template > template.js
stachec php path/to/template > template.php
```

## JavaScript

JavaScript version has a small runtime dependency in `js/` directory.

## PHP

PHP version generates code for associative arrays. Objects are not
supported.
