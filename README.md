# bibmod

<?xml version="1.0" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="content-type" content="text/html; charset=utf-8" />
<link rev="made" href="mailto:" />
</head>

<body>



<ul id="index">
  <li><a href="#NAME">NAME</a></li>
  <li><a href="#SYNOPSIS">SYNOPSIS</a></li>
  <li><a href="#DESCRIPTION">DESCRIPTION</a></li>
  <li><a href="#OPTIONS">OPTIONS</a></li>
  <li><a href="#EXAMPLES">EXAMPLES</a></li>
  <li><a href="#REQUIREMENTS">REQUIREMENTS</a></li>
  <li><a href="#CAVEATS">CAVEATS</a></li>
  <li><a href="#SEE-ALSO">SEE ALSO</a></li>
  <li><a href="#AUTHOR">AUTHOR</a></li>
  <li><a href="#COPYRIGHT">COPYRIGHT</a></li>
  <li><a href="#LICENSE">LICENSE</a></li>
</ul>

<h1 id="NAME">NAME</h1>

<p>bibmod - Modify .bib files</p>

<h1 id="SYNOPSIS">SYNOPSIS</h1>

<pre><code>    perl bibmod.pl [bib_file ...] [-all] [-prf=prf_file] [-verbose]
                   [-nofm] [-nopause]</code></pre>

<h1 id="DESCRIPTION">DESCRIPTION</h1>

<pre><code>    Designated .bib files are modified according to the predefined
    modifier routines and/or user preferences.
    Although any .bib file can be modified, this program can be
    particularly useful for EndNote-exported .bib files,
    where many strings needs be converted to TeX commands.</code></pre>

<h1 id="OPTIONS">OPTIONS</h1>

<pre><code>    bib_file ...
        .bib files to be modified.

    -all (short form: -a)
        All .bib files in the current working directory will be modified.

    -prf=prf_file
        A user preferences file for string modifications.
        String pairs contained in this file take precedence
        over the predefined modifier routines.
        Refer to the sample file &#39;sample.prf&#39; for the syntax.

    -verbose (short form: -verb)
        Display the new lines of a bib file before its modifications.

    -nofm
        Do not show the front matter at the beginning of the program.

    -nopause
        Do not pause the shell at the end of the program.</code></pre>

<h1 id="EXAMPLES">EXAMPLES</h1>

<pre><code>    perl bibmod.pl molytech.bib -verbose
    perl bibmod.pl molytech.bib murine.bib -prf=molytech.prf
    perl bibmod.pl -all -nofm -nopause</code></pre>

<h1 id="REQUIREMENTS">REQUIREMENTS</h1>

<p>Perl 5</p>

<h1 id="CAVEATS">CAVEATS</h1>

<pre><code>    Please make sure that your .bib files are encoded in UTF-8;
    non-7-bit characters will be corrupted otherwise.</code></pre>

<h1 id="SEE-ALSO">SEE ALSO</h1>

<p><a href="https://github.com/jangcom/bibmod">bibmod on GitHub</a></p>

<h1 id="AUTHOR">AUTHOR</h1>

<p>Jaewoong Jang &lt;jangj@korea.ac.kr&gt;</p>

<h1 id="COPYRIGHT">COPYRIGHT</h1>

<p>Copyright (c) 2017-2019 Jaewoong Jang</p>

<h1 id="LICENSE">LICENSE</h1>

<p>This software is available under the MIT license; the license information is found in &#39;LICENSE&#39;.</p>


</body>

</html>
