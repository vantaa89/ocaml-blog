open! Core
open! Import

let print_rendered markdown = print_string (Markdown_renderer.render ~markdown)

let%expect_test "renders headings and plain text" =
  let markdown =
    {|
# Heading 1
## Heading 2-1
## Heading 2-2

Lorem ipsum dolor |}
  in
  print_rendered markdown;
  [%expect
    {|
    <h1 id="heading-1"><a class="anchor" aria-hidden="true" href="#heading-1"></a>Heading 1</h1>
    <h2 id="heading-2-1"><a class="anchor" aria-hidden="true" href="#heading-2-1"></a>Heading 2-1</h2>
    <h2 id="heading-2-2"><a class="anchor" aria-hidden="true" href="#heading-2-2"></a>Heading 2-2</h2>
    <p>Lorem ipsum dolor</p> |}]
;;

let%expect_test "renders code and math" =
  let code_markdown =
    {| The code below calculates fibonaci sequence
```ocaml
  let rec fibo n =
    match n >= 2 with
    | true -> (fibo (n-1)) + (fibo (n-2))
    | false -> 1
```
This is equivalent to $f_n = f_{n-1} + f_{n-2}
|}
  in
  print_rendered code_markdown;
  [%expect
    {|
<p>The code below calculates fibonaci sequence</p>
<pre><code class="language-ocaml">  let rec fibo n =
    match n &gt;= 2 with
    | true -&gt; (fibo (n-1)) + (fibo (n-2))
    | false -&gt; 1
</code></pre>
<p>This is equivalent to $f_n = f_{n-1} + f_{n-2}</p> |}];
  (* Katex will be applied from client side *)
  let math_markdown =
    {|
> **Lemma**. Suppose that the singular values of a matrix $A\in \mathbb{R}^{n\times m}$ are $\sigma_1, \sigma_2, \cdots, \sigma_k$. Then, the $i$-th singular value $\sigma_i$ satisfies
$$\sigma_i = \max\limits_{\lVert y \rVert = \lVert z \rVert = 1}y^TAz = y_i^T Az_i$$ |}
  in
  print_rendered math_markdown;
  [%expect
    {|
    <blockquote>
    <p><strong>Lemma</strong>. Suppose that the singular values of a matrix \(A\in \mathbb{R}^{n\times m}\) are \(\sigma_1, \sigma_2, \cdots, \sigma_k\). Then, the \(i\)-th singular value \(\sigma_i\) satisfies
    \[\sigma_i = \max\limits_{\lVert y \rVert = \lVert z \rVert = 1}y^TAz = y_i^T Az_i\]</p>
    </blockquote> |}]
;;

let%expect_test "renders [TOC]" =
  let markdown =
    {| [TOC]
# heading 1
## heading 2
text |}
  in
  print_rendered markdown;
  [%expect
    {|
    <div class="toc">
    <ul>
    <li><a href="#heading-1">heading 1</a></li>
    <ul>
    <li><a href="#heading-2">heading 2</a></li>
    </ul>
    </ul>
    </div>
    <h1 id="heading-1"><a class="anchor" aria-hidden="true" href="#heading-1"></a>heading 1</h1>
    <h2 id="heading-2"><a class="anchor" aria-hidden="true" href="#heading-2"></a>heading 2</h2>
    <p>text</p> |}]
;;

let%expect_test "renders footnote" =
  let markdown =
    {|Here is a footnote reference.[^1]

[^1]: And here is the footnote text.|}
  in
  print_rendered markdown;
  [%expect
    {|
    <p>Here is a footnote reference.<sup><a href="#fn-1" id="ref-1-fn-1" role="doc-noteref" class="fn-label">[1]</a></sup></p>
    <section role="doc-endnotes"><ol>
    <li id="fn-1">
    <p>And here is the footnote text.</p>
    <span><a href="#ref-1-fn-1" role="doc-backlink" class="fn-label">↩︎︎</a></span></li></ol></section> |}]
;;
