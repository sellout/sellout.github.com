---
layout: page
title: Wandering through the technical landscape
tagline: and rapidly running out of metaphorical water
---
{% include JB/setup %}

I am a hobbyist and professional programming language designer and compiler writer. I currently am paid to work on <abbr title="Free and Open Source Software">FOSS</abbr> by [SlamData](http://slamdata.com).

This blog is a _blog_, not a textbook or research publication. I’ll probably get things wrong a _lot_. Please (politely) challenge my assumptions in the comments or feel free to ask for references, but don’t blindly trust anything I’ve written here.

## Posts

<dl class="posts">
  {% for post in site.posts %}
    <dt><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></dt>
    <dd><span>{{ post.date | date_to_string }}</span></dd>
    <dd><span>{{ post.content | strip_html | truncatewords: 50 }}</span></dd>
  {% endfor %}
</dl>
