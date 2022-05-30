# Niao

Niǎo is a small and experimental, toy language (for now).

![](https://s3.us-west-2.amazonaws.com/secure.notion-static.com/b6ae56e7-b021-42d9-be01-da0586680712/%E3%82%B9%E3%82%AF%E3%83%AA%E3%83%BC%E3%83%B3%E3%82%B7%E3%83%A7%E3%83%83%E3%83%88_2022-04-26_0.43.15.png?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Content-Sha256=UNSIGNED-PAYLOAD&X-Amz-Credential=AKIAT73L2G45EIPT3X45%2F20220530%2Fus-west-2%2Fs3%2Faws4_request&X-Amz-Date=20220530T100715Z&X-Amz-Expires=86400&X-Amz-Signature=45e66657677ca671815bb7601550e6d4f50dc190222fec54a3c4e68f4263172a&X-Amz-SignedHeaders=host&response-content-disposition=filename%20%3D%22%25E3%2582%25B9%25E3%2582%25AF%25E3%2583%25AA%25E3%2583%25BC%25E3%2583%25B3%25E3%2582%25B7%25E3%2583%25A7%25E3%2583%2583%25E3%2583%2588%25202022-04-26%25200.43.15.png%22&x-id=GetObject)

Niao is aimed to be compiled into multiple backends. e.g. BPF, WebAsembly and such.

## Language overview

See [Memo on Notion](https://frequent-color-40c.notion.site/Niao-language-overview-and-spec-0fbb5643e5ab4865bd7198fdfb1ee0ea)

## Example code

```
fun max(x : i32, y : i32) {
  if x > y or x == y {
    return x;
  } else {
    return y;
  }
}

some := 1
other := 3

puts(max(some, other))
```
