# The about page's HTML

The about page shows HTML that is uploaded to the blog, not written in it. Whatever
produces that HTML, `static/style.css` lays it out as long as it follows the structure
below; the CSS selects only by these elements and classes.

## Uploading

```
PUT /api/about
Authorization: Bearer <token>
Content-Length: <n>

<!DOCTYPE html><html>…<body>…</body></html>
```

- The token must belong to the author of the post whose slug is `about`. Tokens are
  issued with `blog token create`.
- The body is a whole HTML document of known length, at most 10 MiB. Only the children
  of `<body>` are kept; `<head>` is discarded.
- They replace the English content of the about post. Edits made to that post in the
  blog are lost on the next upload.
- The HTML must work as static markup: scripts, embedded objects and frames do not run
  on the page.

A PDF of the same document can be uploaded with `PUT /api/cv`, under the same token
rules; it is served as `media/cv.pdf`.

## Structure

The body is a flat run of sections. Each section is an `h2` followed by its items;
sections are not wrapped in an element.

```html
<h2>Publications <span class="note">* denotes equal contribution</span></h2>
…items…
<h2>Next section</h2>
…
<p class="updated">Last updated: Jan 1, 2030</p>
```

| Element | Meaning |
|---|---|
| `h2` | Section title. |
| `span.note` | Optional note inside an `h2`, shown small and to the right. |
| `div.item.entry` | An organisation and the roles held there. |
| `div.honor` | One honor and its year, on one line. |
| `div.item.group` | A label over a line of items, such as skills or courses. |
| `section[role="doc-bibliography"]` | A numbered publication list. |
| `p.updated` | The last line of the page. |

A section holds one kind of item, though nothing enforces it.

### Entry

```html
<div class="item entry">
  <h3><strong>A Company</strong></h3>
  <p class="location">City, Country</p>
  <div class="positions">
    <div class="position">
      <p class="role">Senior Engineer</p>
      <p class="time">Jan. 2030 – Present</p>
    </div>
    <div class="position">
      <p class="role">Engineer</p>
      <p class="time">Mar. 2027 – Dec. 2029</p>
    </div>
  </div>
  <div class="details">
    <ul>
      <li>One line about what was done there</li>
    </ul>
  </div>
</div>
```

- `h3` and `.location` share the first row; the location goes in the right column.
- `.positions` holds one `.position` per role, each a role and its period. On narrow
  screens the period moves under its role, and the positions are spaced apart so each
  role stays paired with its own dates.
- `.details` is shown small and grey under the roles, one line per `li`. It may be
  empty.

### Honor

```html
<div class="honor">
  <h4><strong>A Scholarship</strong>, <em>A Foundation</em> (full tuition)</h4>
  <p class="time">2028–2029</p>
</div>
```

The year stays beside the honor on every screen size. Only the parts in `strong` are
bold.

### Group

```html
<div class="item group">
  <h4><strong>Programming</strong></h4>
  <p>Language A · Language B · Language C</p>
</div>
```

Items are separated by ` · ` rather than commas, since an item (a course name, say) may
contain commas itself.

### Bibliography

```html
<section role="doc-bibliography">
  <ul>
    <li><span class="prefix">[1]</span> A. Author, <strong>B. Owner</strong>, “A paper,” in <em>A Conference</em>, 2030.</li>
    <li><span class="prefix">[2]</span> …</li>
  </ul>
</section>
```

Each entry hangs from its `.prefix`, which sits in a margin of its own so the text of a
long entry lines up under itself. Mark the owner's name with `strong`.

## Layout rules

- The page is plain block flow. Items space themselves with `margin-bottom`, which
  collapses into the next `h2`'s `margin-top`, so sections are evenly spaced whatever
  they end with. `.item` (entries and groups) is spaced wider than `.honor`.
- `.location`, `.time`, `.note`, `.details` and `.updated` share one secondary style:
  smaller and grey.
- `.entry`, `.position` and `.honor` lay out in the same two columns,
  `--about-columns`, so their right columns line up. The right column fits a period
  like "Mar. 2027 – Dec. 2029"; a longer one overhangs to the left.
- `h3`, `h4`, `p` and `ul` are reset to plain text with no bullets or margins, so their
  meaning comes from the classes, not from browser defaults. Bold comes from `strong`.
- Links keep the colour of the text around them.

Renaming or adding a class means changing both whatever produces the HTML and
`static/style.css`.
