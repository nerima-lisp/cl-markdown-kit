(("html/escaped-paragraph" . "\"<p>A &amp; &lt;B&gt;</p>
\"")
 ("html/link-image-attributes"
  . "\"<p><a href=\\\"/target\\\" title=\\\"tip\\\">link</a> <img src=\\\"/image\\\" alt=\\\"A &amp; &lt;B&gt;\\\" title=\\\"photo\\\" /></p>
\"")
 ("html/escaped-code-block"
  . "\"<pre><code class=\\\"language-lisp\\\">&lt;tag&gt;
</code></pre>
\"")
 ("html/tight-list-items" . "\"<ul>
<li>first</li>
<li>second</li>
</ul>
\"")
 ("html/aligned-table" . "\"<table>
<thead>
<tr>
<th align=\\\"left\\\">Name</th>
<th align=\\\"right\\\">Value</th>
</tr>
</thead>
<tbody>
<tr>
<td align=\\\"left\\\">x</td>
<td align=\\\"right\\\">1</td>
</tr>
</tbody>
</table>
\"")
 ("html/strikethrough" . "\"<p><del>removed</del></p>
\"")
 ("html/escaped-raw-inline-html"
  . "\"<p>&lt;em&gt;safe&lt;/em&gt;&lt;script&gt;x&lt;/script&gt;</p>
\"")
 ("html/escaped-title-attribute"
  . "\"<p><a href=\\\"/safe\\\" title=\\\"safe&quot; onmouseover=&quot;alert(1)\\\">link</a></p>
\"")
 ("html/empty-document" . "\"\"") ("html/non-rendered-block-nodes" . "\"\"")
 ("html/escaped-raw-block-html" . "\"&lt;b&gt;raw&lt;/b&gt;
\"")
 ("html/gfm-tagfilter-boundaries"
  . "\"&lt;script>x&lt;/script> <scriptx>safe</scriptx> &lt;script/> &lt;style type=\\\"text/css\\\">x&lt;/style> plain<
\"")
 ("html/omitted-inline-html" . "\"<p><!-- raw HTML omitted --></p>
\"")
 ("html/passthrough-inline-html" . "\"<p>plain text</p>
\"")
 ("html/passthrough-commonmark-inline"
  . "\"<p><script>x</script><style>y</style><textarea>z</textarea><pre>w</pre></p>
\"")
 ("html/passthrough-commonmark-inline-stream"
  . "\"<p><script>x</script><style>y</style><textarea>z</textarea><pre>w</pre></p>
\"")
 ("html/passthrough-commonmark-block" . "\"<script>x</script>
<style>y</style>
<textarea>z</textarea>
<pre>w</pre>
\"")
 ("html/gfm-inline-tagfilter" . "\"<p>&lt;textarea>x&lt;/textarea></p>
\"")
 ("html/gfm-block-tagfilter" . "\"&lt;textarea>x&lt;/textarea>
\"")
 ("html/inline-rendering" . "\"<p><code>&lt;code&gt;</code>
<br />
<em>em</em> <strong>strong</strong> <a href=\\\"https://example.test/?a=%22x\\\">&lt;https://example.test&gt;</a></p>
\"")
 ("html/block-rendering" . "\"<blockquote>
<p>quoted</p>
</blockquote>
<h2>Heading</h2>
<hr />
<blockquote>
</blockquote>
\"")
 ("html/ordered-task-list" . "\"<ol start=\\\"3\\\">
<li><input type=\\\"checkbox\\\" disabled=\\\"\\\" /> \
<p>todo</p>
</li>
<li></li>
</ol>
\"")
 ("html/empty-ordered-list" . "\"<ol>
</ol>
\"")
 ("html/ordered-list-start-number" . "\"<ol start=\\\"4\\\">
</ol>
\"")
 ("html/code-info-tokens" . "\"<pre><code>plain
</code></pre>
<pre><code>blank
</code></pre>
<pre><code class=\\\"language-lisp\\\">marked
</code></pre>
\"")
 ("html/image-alt-text"
  . "\"<p><img src=\\\"/alt\\\" alt=\\\"text &lt;code&gt; &lt;raw&gt;  auto nested strong struck nested &amp; image\\\" /></p>
\"")
 ("html/unresolved-footnote-header-table" . "\"<p></p>
<table>
<thead>
<tr>
<th>Header</th>
</tr>
</thead>
</table>
\""))
