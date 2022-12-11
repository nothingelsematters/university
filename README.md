# ITMO University Computer Science course homeworks

[Rust]: https://img.shields.io/badge/Rust-grey?style=flat-square&logo=Rust
[JavaScript]: https://img.shields.io/badge/JavaScript-grey?style=flat-square&logo=JavaScript
[Kotlin]: https://img.shields.io/badge/Kotlin-grey?style=flat-square&logo=Kotlin
[Scala]: https://img.shields.io/badge/Scala-grey?style=flat-square&logo=Scala
[Haskell]: https://img.shields.io/badge/Haskell-grey?style=flat-square&logo=Haskell
[Perl]: https://img.shields.io/badge/Perl-grey?style=flat-square&logo=Perl
[Python]: https://img.shields.io/badge/Python-grey?style=flat-square&logo=Python
[C++]: https://img.shields.io/badge/C%2B%2B-grey?style=flat-square&logo=C%2B%2B
[Octave]: https://img.shields.io/badge/Octave-grey?style=flat-square&logo=Octave
[SQL]: https://img.shields.io/badge/SQL-grey?style=flat-square&logo=PostgreSQL
[Shell]: https://img.shields.io/badge/Shell-grey?style=flat-square&logo=gnometerminal

[Java]: https://img.shields.io/badge/Java-grey?style=flat-square&logo=data:image/jpg;base64,iVBORw0KGgoAAAANSUhEUgAAAHgAAAB4CAMAAAAOusbgAAAAbFBMVEVHcEwydcNjuu1fsOm759t70/U0e8nN7M2q3uYucL5xxvBVp+f+v2r7XSo4f8w9idFDldlQoeQ7hdA/jtNIl9pMmt3+plb8ZC39ajH+hkH+eTv9bjT+mU3+gD7+kkb+djf+hD/CtJ8ubbj4WCr2bN2zAAAAIXRSTlMA7EFWCR/fAxH4MGcf8NK6n3bErJGFON3Mc5q9UYtiq36O1m1KAAAF0ElEQVRo3u1Z2ZKjOgxtG4M3AmbLvnTB///jeIMAWZpAbKpuXfXLzPSQE0lH0pH4+fmSYfKzjsXnlYAPh3Vw0Wkl4O3mF6/i8KW5rAIc79YBxodmHWDpcHNYARgdm6ZZo47Pm6bZbNcJdLOP/Qf6JnGbI1qD0TLS51USvEaktyrBKxRTfGpWcZgcm1UcxofNKg5jQyz/lLbE8l3DOLa4vgMd7w2u7y4d35pVGN0W0ijBGDnW1+RicW9xP+vbw82tEkEXU0jN6Y5Ltpf9pjk5zTj6fcCNryf1b0enDG8b1h03PmiKbw5uK/o6wkVXU9H7rdP8You7ORoC462ZUJuLW0J3DdqGFdnvsTs7rmfboFscYvl9ct2/tvtBOuOjDbvrft0qjm1PyrtPbytlO9y2ro6ucfGvye+224pNfp3jWqBzdwYYfg93k8EQ69jWzdn0Efe3gFGgsW0czgWIlTqdw7HNsHPFdRhJyqudyK4jbSO76yJrtcAR+4n0vq0dfPPksQXuUmqLyX2Ox8A29O5ZbT3sQv3T6turpzIek8tbkrtyso3L/SqBL08biI+C2g9i3bLLw5Qwcus6al0eXDbzeN8WFNn5yvIPufVdNicuZe5Pt1pzdVluV2QPuznWKrNzsN0pPCzn+Lzr4ZCTr+5lN5gbGa4zjY9XIYpSm+soy15eDKiiatlku5mnNxIKrT252BXO0+sudNm0JWUmh7fbntwRLZ3QsH+79/lgiW3kgZujIoE8gw95jvF9XX42nnDIIIThbNIhWCaA0gK/Edub65PfhiICAEQCzoImQQpqaQC+XtR3t+c3F8J4TuWj5ee7JAoSqmDrKHhdWoS8dolU8nmahp8GOTWwdQJnJ0qo58VHPoelha1zNp+YUGWKBh88wXILKx8bxRJjJI0oC3sm/4oeok4i9RHpBy7zurOk4kEQwEAZ51lVlaUQRZrneZIkkTX5xzwV1Tg4JHnPzWeRBndo2rf6pdFEjIFD7XGdfVBTiGUpqCcaBUlachY+NDBovmf1WTGjMBDJaw+l9yBKZHx5wMIn+VV0KC1NZhQE42WRd6mMVCrTQpSZzDuTjHrrCraRTsKZ1YhaAivmIjQ5brjSuCDw/cpTV3FNK98vtcNU44qZhz9EZgYKlYtwQ5ELzmZg40wVBC3nHjpZpMo0rwIoKfwJPgfL8kvSrknILiELqcoybi3T/VNZJcuLDXKiiQUytJSaU9pXxIeBqpNFdYSDaCpwMIgTLdjCsmApndCt86zXnzitI778fk60evpjQgxgsrxiX2lXBFapVJtDPDki8qLkmu/fKv7HTGMihbJUApkyLkcEZHImEeSpDWNjP//bf88w0kqAabtrAtfJJoGRP6A1pYFy3bhlMSnB5chd8afiKzMYugCe1KnF96HDfMqQAMxBkqvozzkBsu9QDckOyXoKWe4V9G2g4TdwsdohAE3xcMMXafSArsiVlkH4DWKru4f6/Ag+rP8hDHhVyl1RLouprScWfmcUkaAA9hCAXzQTux23W8WbW8T0oYwCKzfoF6oDptFUprPC5pBWi4ULyaKaTlzJg05RpovIgtWGqwR5Oe1jwruejHg4ZwRgNU0gF7lW8wmf+PVZ1K/Monq5cD/CqeklpVEmOmkGUj6ZJThIRnouyoUZQHY17sxuzMzoMFlgcnqB7kBCozSD5LPGUURPWoTSk3IMShPyR1oqCzkxg3J8j6FRXo7WmYm0gFmRAFrPMKnqi2wgdjFhcPrANASRsz+aiq9yUlR8uFBKZisxDqhAnzqvOmRWSb6o9A0jqk9eKgOpqDIl6QckVHGrcvO1Z3cirEtEk8io+YGgH4ps+18zkbexAgV0IsyUutcc18XEdXRoj2flclhsK4ipi3972FSruZpVWgk+VKIclgthseRaJe5C891dUx0OWqIt11zFn/TWRCtEK3e/lFccMtkMTUSlgf5xUV8XoWJaqBQ+dsMi0jclA/TqOAftH9a++HOyQ+yPAAAAAElFTkSuQmCC

[Lama]: https://img.shields.io/badge/Lama-grey?style=flat-square&logo=data:image/jpg;base64,iVBORw0KGgoAAAANSUhEUgAAAHgAAAB4CAYAAAA5ZDbSAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAACXBIWXMAACxKAAAsSgF3enRNAAAAB3RJTUUH4ggHDQ8aSN0VtwAAAAZiS0dEAP8A/wD/oL2nkwAAG8ZJREFUeNrtXQd8FNed5lJ8lx7H6cWJL7ncXS652I7txKm4xj47sUNspzlO3BJMDJicW7BNFyDUe5d2V2XVVr1Lq4aEKBIIJJDpvQtMB2ln5n//7+0bMSoILWh10mrm9/s0s7ujmTfve//6ykyZYm7mZm7mZm6TbctMiNDxZcZjjKmMj+rfj9K1v8D4BeM+xsdG49rm5hkJ/85oZiiMcwyrJGU0yL2b0cLoZVxgxOoNyNzGTnr9GZRvi1PyLDEqjhkWg7S9TxJ+O+M2xuclSV9hfIbxfiNhhut+i7EhKzGSCvjajhRxbZD8qEnw2BEMohqyk6KoOteqApIISNxcxmcZ79jjIzbb48PfE0iI6ODvGvl4J2MTHwcw7mB8g/Fhed2bGIVoLIWpcaozz6aVZSbpjWexqabHjmDY3u25KdFUkZ2i5VuiqDg9QQXh/H03YxXDlW+Lp2pHmgBLOuUmR1OpPVktSI3X+HeViT7B2M/HBYyfMpbje0dKtFqakSiuW2pPVCDN/H2a1AomCWNA8H8yjjgsMZojOUKb/8oLlBC0kApY6rISI0AeE2/Ttq6tU/dtaiFg65o6bXNztbqnvVnd3tqgOvMz1LIsi1qWmcINQxB4FLacj7XCtHgt1n8eLZzzIjmSI0k2nErGP5sEjw3BsKsn8iyxvWnRAeprL/6O/jHjGbJF+mss1QpLqwpyD3Suof0dqwUOdAJr+o6ZaG1P+ypBeHO5Q5VSqsGep4QtpTenP02v/+X3lB4dqBNcz/iQSfDYEPw9xkmW4F4mQ331hd/SnD8/QZBktp1aS2V+L0utqpN7NezduErbUF+m1uSlsXqOpHmznqO/P/cUoeFYwpebBP8/qeijLK0sbcu0uSy98SvmU2ZsEHW11NBIiTUC0g1VvmVVNUUvfUsQDAl2Eyyku8pU0WNH8JfgZEGyUqMCtOz4YKhcJshNlm53PYeb6NaaQmF/33zpj5QWHag7WemmkzV2BH+EUSfiYGus2tlcdZ3E9gerbFpd4aBIv7mUGR+myDBpgRkmjW2iYwUqvjg9Ud3Z1jhq5BrBNlmEU4yLZqJj7Al+EZWfmxKjdbU4tdGUYGAPS3FVbqrGcTKkt00mT0wCxojcuxhdsI2VOTata7VThf0cbQlmb1wPn1YyPmESPDYE38Cwo+Kbyxzq7g3N2t6NsJujj13rmzQkTFiKT8kOCJOEMSD4ZuSTi9OTiG2vCofIW2Ap1uqLspDShC0OY3zSJNn7BH+DK/wA0oy7NzR5lWAkQJA0ybPG6s5WhBkLe59g9APvy7PEEmzvQFLgHO1pl3vj56GwcYjzBl6vvVnb0uJUSzKSoarf43v/xCTYuw7WQoRH1Y40dfu6Bu0yYc2CkINdbXRsxybq3r2ZsYWO7+qkYzs76Oj2jXR4Wzsd3rqBjvD+KJ+D74/3nbeZjvA5+zvXDmoIsMctFfmajIdfMgn2HsE32hMi1uVb4/qkF8QK8DHIPH9sL108cYB6Th2mnvcOieML3fsv4/i+fp/xe688F5/PHtlNh95tu3xdiZUlObDFUNPPmAR7j+CvcCXvKLEna7vWr1SNBBzYvJbOSXKPskQ6C+3UUlNEZ5iwfgQPIPfwjg6qzk+ntXWldPboHvHdewe2kbHxoDEVpiZARbu4DE+bGS3vEQwvdg07PbRlVY0gmMMkRpNQzRe699GpQzvo9elP0zc+fQN95+ZPki06YLAUS3K7975LLz8zjb5+0w106y03Ua4lmi6dPEhnDu0Uallcm++xs22ltrqqQM1x9yrVmTGxd22wH2xhTV66wmGSBnKBA5vX0UUmbtuGVXTnN79EX/7oFPrCh6bQn371c5bMwVIMIje1OOm7X/1037kz/jiNzrMKP3VwO7kbT1MfWGNwTGyFFJ+QY7xMUrxEcBAIbi53KFzxfQTv61jNKnoPHWP1/IdfTKUvfngK3fzx95H/26/QRSbz0qkjjKMGHKGD7FT96r67xLm33PgBilr+jiD+5L53+5ErocGxY4JP8/2/bxLsHYIRg5Y5UmJoS0uNAhW6C9IFAtjbPXVwhyCzq3UlhfvNJUukPx3bt516L54nTVOJ/zDce1VVqOf8aepYU08hi96g9LhgOs4qG6ob3vWu9U19197BmqLVWYwRllDR7XKEpkmKFwjGUNd09AO31ZYogoT1K/uAcEdXv/CKXedOkOLqpcLCQlq3bh3zygTzpqoqNTY2ktPpJFJ62OM+0udxQ0Uf3NJqvK7WWJLdK0d1IGX5Z9PJ8q6KfopxPs8apzWV5qosXYKInesbRXhzvi8MOkDKpfOC0M7OTlq+fDnZbDZBLPYrVqygXbt2id97zp7ss81nDu8SIZeBYJVVcy+rZgzJnTlwPLW5jT7J6GyYhwovTEtUt62tV3eC4LaVghgQJLzkk4dIU1ykb93d3VRQUEBz5syhsrIy8VnfXBfP9RGMxId+PQGWYJmPJung/ZNJsHcJ/hdGIFe4Upye5Nq6tk5xk9EoAPs5FMHYjh07JiT57Nmz/b7XCUYcjUyWfi03VtLm5mqtIDWe7AkRu+VAeZMML6poJBouFKUnau2N5UJF62TgeA/Hr5Bi2FOl5wL19PQIYnt7e4V6fvbZZ4U9PnfuHO3du5dcLhf1nnOr6O49XVJ6GwdCqyvMVKSa/rVJsPcIxrwjZ05yNK2vK1UhXTtaGwfh0NYNwhb3nDlO7508QfHx8RQSEkLh4eFUV1dHYWFhFBwcTHa7nS6cOy2kXdjeTauHvB7u01zm0Ed3vGAS7D2C/4sr+WhZZgqx7dV2tDbQUBCqelenkMqeM9105tRJOnz4EF0473a6zvO++/gxusjkIh4+d3QPHWDP+UrXQ6cGEiswC1yG35letPcIvpsr+WyVI03bjoofBjtY6tBjBLt6iSW0l6UZEn3p9DGx7zl9VKjxM4d30n4md7hrdTZXw97rk9CKzPFZ3iP4FiZ4N0ZzvLu6VmPJoqthHztN3Xu2CBWMTBcIh8SePrhDNAAkSLavqx/2GqwtqKOpSq3ItoJkdBu+bkqx90IkB5IO6+tKVCFhgpyhsU0CJO3c0MQ2tkXY2d0cTsGubut3zpWvIY75XhsbKzCKU5/G8hGTYO940bPg7LBNVLeyHYZ0jRUwsqMgNYH4/u1yLrFJzCgTjCRDJEZUIou1bW0djRUgxfDc4cFzGSrMiWjeIfgTen8w20TM/6UxwZo6am8oU4szhKMFzDBtsHcI/hSWX0BWaXNztaj4scI6ZzHUsyadrDdMgr1DMLoLS6AmW2tLMMmb3l1TOyYAyVDRGM3JZdjK+JpJsHecrOew2EpJRjK115dpHC7RGAIJD3Q89HAZfmkS7L10pQ2eNJOsdjZXC5K7Vju9DkhyQ3G2ItW0ObrSiyT/CUNYIU1dLVz5YwcNk9343lg36wGTYO+p6UVIG7ZU5KmQLI5Ph0VXy9XPGck11tUUKTJMOsn4keloeY9gP8TCLRX5iiBvVc1ltEgYvsMKAJtX9f/uWrC+tlRhCdanlGaZsbD3SEafsKsqN1XYYJCnE9hWV0rr68vEMcfK/H011Rdn0ZrqQvHd5usAGs6mlZUax8NQ08e5DLd6SrBhbNlNcqnFj5iaYHAFYZWd/VKKXYI4jouxryvKFIR2MrlVeWnUUJJN5TlWDLMl/bzrAjcYOfP/kmxo/yaXdbpbrnoL/JBxpxw/jbJ+XS4ccyPjB4x4rIfJ13gX/duMV+T6mV+Utn2a/P/JRb6h9WNslFKUlqi2cTyMiu+UBKytKaKSrBRBainvKx2p5CzMQEcB6eddK/D/HAtrBbZ4EAwcQf+0XA/zNOOcxBlMGmecZBxj7GNstTOpWKEPDTPPGkfIa0ub7pKr23bweRfR74z/5c95jO9OGpLlg0ICjmEdSpDrrvyqy2DJbXUWY/kFam8oF58Fmq8fHU2VmjM/Q3FYYojvrxSlJ6ql9mQqz7K4KrJtvSzZApV8XJFl7eXvFfyO7s3CtAQ13xqnAQ1FWSoWXuNGp7HjJpZThNOInip46c68dGEGpK1vk0tWQKV/wGeJHjh9tKE4S5ALOzsQbkKrxb5jlLGxoVxjKUbjUTc1Vqhsk0VeHOiU6DAAvwNMpgJSGX1l7JDl5O8QfqlrqgrUDtkYQX61I03E3CzNh1n6sUJukiTb9yRaPtQHGflQawhZRKWjgscSTZWS7MrLuOr5RlQNeQ43Fq3ftfg8pEYLUxMUNgmEKbNSoncx7vU5kuUDISyphipj9ezqkNLhs2isFFNmIOGw/XWFdn3JZKfPzXA0OFh2PCSmc7oJrvBxXCYc5gFzo+VSEnf5IsFTZEghRnTAM55MgBpnJ84lR3e+4FNq2kDw83hAhEgbWG2Jh2dveVKAn7WxOFtX07v0BWF8gmR9jQ6sOocHxJAdhEGTEbUFdj2E6sIQJsaz8jUHE5dsWfh7MC661J6CcENrr+cHnoTYUFemVXBYxQ1dJxoqu1W+d2JikiwLPh32tzY/Q0FLRkw5WdFaW6K1VOSr0GQYry0XLW+XMy/umHBpTlnYOSC4vjCzRzxoXamPQhJZV3b1cxgIocqzrEhvkkhzurszkeb8zoQhWRYUs+vFWhl4yPU+CvR8rarIo1ZnyYjOB9nsfCkwXegIYQdUV92NsgNjwhB8B7fO41izal1Nkba+livAB7GyNEd0llTmpiJjN6L/aXUWa23OEpFGRd3I/LY2YYb4ykJ+HF60HPjuauMH81Wgw4QdKUG2p/+7vrZURBlSii0T4l0TkmD0kR5BXnZ1ZYEGFYaKGBlKRJ8wepk8+7+xQZsoU0lf2VBOdHeizG0elrdNaIE+gvFmtyfHNcmGJMcSfRE0TysQqg4V1liSM+7Iba0pFjaXGy2tZfvrLMigksxkqi20c7mv7ZqryvMU91vdRFLkiHxN7rgmGG5/LXqSVpU7VDw4SBspUHGotMaSbFEBa6uLhDOD66ypKhAV7Mn1rher+Z5NZbmkP0dZtoWK7EmiEVbk2jBShTx9xiGA/mtdknPk7MxxSzDGL3Whw5wJUcQDVHsAPt9ZYBeVWJOfLioSjowuMRj54fE1rxV8HxCoNzgAx2w3MVJUlGlUyoKGVFmgYskprrst43YBN0nw53WCV1fmK6gET7GmqpDqizLZO7UJklGpxfZkKsxIFJUMqb6W614rGoqzheSWZlkwgMEL9ygSQ4sd7uk2G+W4r3FLMN7xW40VX7F2s06YpzBWAKQIA/SayhzXdK3rxVrs2e4K2+uF62MRc7kqAQiOGbeOlsHJQiZLw5QVFB628/pRSKNznfEF9sLRd6yTu0nPao33MOnz+ivtKrJt6mrZ+k0MDUzQk2t7PTnukx0GKZ5rZ4JL7ckK22LYYzIxGGx7tcLUBP2N6LdOlERHX39wfWEm1DSZGAwQXFvQN36rXmYAJwTB6O88g2Q6x8KMPDIBOLDyrtJclqvAAUVnjFwN6Bzj9xMpF/2c2/5aJbkOE4yVpTkqN/rePEusKzclWstyO1Z49cCbE+ZFXpLgv+oEI0fbXDZ2QBICGMt7egANK/HJtTS7ZEh0z4SaCSEJ/rlYaZZVNFe2xipJpPtGG82GvRsO5L9FgsRb97y+8jowfaZXrgD0/IQctiML/WlGM3Kr5VlWtaE4W2Wbo+nSNRpoKMoiZ36GOK4rtAvwPag0M4XKsy00mvcaTZRlprjk2KzfTOQxWVPk9MrdWe7JWhi94Gp0E02jAfY+qSAtnqocqWKflxpHBekJVJieyGRn0mjdZ8Qovfo5jSXZWLAc+eb35OS8CT82+i7Zkd2NUKC2IEOVD3r9KM4WvTkgtDzbKgh35qdTPUv2ytG4/gigPwu0SZUjTeSphzsX3ady0N0aGUr6xBhp5FUXw+nigF5FZYCc0YKo1FG83kjB8T3VcaPCMTdcyrXGCPs/3P/U5meoue55xtsYX/UVgoG/g+DE0OViRGGNI510orGfKICdB7FAUUYi5bNpAMkcz1IOE1yd55bi4a4BxxMOKON+XyJ4AQiOCVjSG7LkHYoNXEK2qGCRg9UfvH68Q5CaJGw9AEIhtQ5brNjrtn+4a+A5i/ka0sl6zJcIRuJDTQz1V0FwMLD4bYpcvlAsjcCqy02ylI7xCkgo+qYBSK2TVXJZlqXPBxjBNbCsBEKkM/ryThN+kwT/B2NvWkwohS9bIEjWEeo3j1IiAqnUniLUnbBtCHsKxiFE2fqj3vh5mP/FeVhtSOadW8dtp/41EozXzbbA/kYHLO5HsI4IlubU6GDEiUIafAnSTmt51lhNLuQy29dmG2IY7QlrVJDGEqsNRXCIVNvJ4SuENwq17SvAIi/sPWsyg2XxqVn/hvcYKrDBoX5utTwUQvi3DJZyZKh8CXjzmxyxkaq/YsDXCH4cSwsnhYHgeTQc0H2GSoEUewR2erB3jvB8LIHkHPp7Ecbpx0bUXAPwfyUZSZok+G++uuoOVpE7kBYTwrZ2gRrGRF4J2YmRspLTPEJRWgJGRqiV2TaVP2uCpGHO5xDNVZ5pUWr63ct9DDLgD2DdLFavVMrhHEI6T8t0uWyJLqme/+CrBGNRlghUnCUyUGWHSg1bOp8GQXjUAfA2RRjiARB+CDWIydYIRTBUqCzTIiZ3MUlqdW4qZjtq2PNvmKerpcWGqniZNDcKcU+QaU8Ip3Aui3D8li2g6BWL0SgJjdPDMrmRm4ZF1UDuWcaPfZVg4CUQYIsKUjn+1cKXzaehgNgYlc5EiEofCXBuofs9DSQT+VifUqxXlSX3eZZYlYlHZ7s+g4DigpYKbcJ7ZNmQiHH7AiJ8c0N3AAXBHpRJBzSKXAaxa8JMEb1GgpMRJsUF+TG5C4R0DAS+j/JfRMjXVuVwBeXYRohUqMFeSfACmSXCOpkJjERGkyT+glxD+ribYD+tf0x+ZQcwIWQZuj49Lhc0iWxQNp98abXhPUoZIDg2cIkGlXcl2Nj2QWV6BK7M4vTEHknw3w2NyhiH3yEndt0mnR2FnT7lSoQKn2DpZaDx5aZEi3t5UjaZuVL0Vwv4sgTjFTewsSpLqRbpv5CGAmZEoGIqsq0CbEdF8kP/fCVwRSrSkRn2HQ2yLFhaeGd6bChFrVikXiZz/jCYB//hquUwgm26JtXzPnnPKT65yUrFWsybIMUsOS6uWI1BA4HfRQWxw1PBKhFJfCT32eMVThC+A3Csf+bfNNkNh+mX/z0CgqfIF1e6UmNCuByLhdMH52qwX7BAYj5ZI4Mu31Pe/0rAORweGWcMfnAyrEAbIrsN1eiAxezU9Ae+g7ODgeB6RWGZ31yr7LFJS0ClCYkWhEvSMZBNVmSRfLX8SMoDidqfHhsGT3mwXzDAdMD5yxZDkCyX7z0c+Lx8W5z+5pe/+vQ60gOkRqjpmMAlottwKCSH+TPJ8YJIACRDkvXuOXzWfwOw0qu0v2+OxM4Z1rM+bRVh2wJNJ7EfDKYjNtBPNDzdZFwNpfYUXT0fZnzb5xcKNwzhOZIRFyakmL1YGowl3ABWwGkSPUz9kSxx+bsyhrS/qMxZHhD8fUFwVJDCBAqfIKoPi9wwmA5olpL0pCHKNDRKMpI12XO0k3HzZCEYYcJcRg87OFpi6DItPngp9YefiDmRPRopmGDdg37HA4IxQW4jGhviYZAY3Qe3uTCaEYRJRSzBHpRL43geTt95OfZ5is9vhvnDUSDEGhXoSuSKY6L7gIpktSnUIeztSGAgeIEHBOsmQ2NHS0X4JsgMBAabDWgX2PkSkbYcWbnyrHG6Znlhsr2w42Fkm1JjgnuSwpZrDBqI9NgQQTLywVdDniVWr8jpI4015Xm/RYyKbsxYNg3AIJPBGiVegr1/kYSB+RhJufKtcXrDe2OyEYzJaefTYkPUlAh/GoRwf8qIC0UHAjJUw0G8WUUORT3kyfRLWY6ZIj8eESBJdJuJBCDECNYuIW7tAv+AvWMqHr5c4nfMQZIEz5hsBCMm3oWY1xYVqFojA2ggcpOj3ASnXgH8GyqQ1aYmM0VLPEkFGvqpXanRwSoTqBlNBZyqpNDBmiU1Okh4+FcrG3LjOclRmhx/NXWyEYyZdKVo3ayKFVt0IFdcf0AqoaKHA1KHUkKiPZ1bK8vxFT35wv6AmhzuryFE471Aig6WWh3QLFcrF7RKbkqMbjZK5JtYJxXBeOCVmQnhTHCwkhYTTAKxEnxs54p0WGKECsbbTIZCjnsYzCU52e1aY3NMAutlb1q1RAaoFiZRIBIwaJUoAY0JvmJ5JBD/6uR2yBz4pHsrGrJN5agEe3yoysCepSOEiQ0Rx6kxQfBuFQc7UFgS0Qi9IrPd2Svkeb95LRUoy4Ll9s+COBDIJoMEhCYJQgMUDS4dYJ+BGyXeLq4NLJMROUlResObNuned2iQnBlZiRGupIgA7fXXZmkzZ/2V/vzCM7Ro3usUvmIh/eWl52neW6+hQ14psPWvQFtMKAX4zVfTYkNB8Lprnecjy4HGsRdqGgQymRq0CBpaAtvc+W+/SsHL5lHo8gUUFeSnZslBBf1JjjUgDgkOVXZL3jEpX2ap2z+WwG0RAUvohz+/X7vtnp8ScPeD99FPHn6Qbufj2a/MYEkKUWPYAUIMmsqEvvn6HHpo2i/pRw89oPkvfgdSjL7d26+D4PfL9xyL/uTkiEBl9isvuV57dSb9+vdP0R33TaUfP/wA7kfTZ7woygEEL1uocCMTJBcYkBEfrrLmQcPbIW38pCX4w1xR1SmRQfTIE4+rt039CX3v3p/R7QwQfdf992hLF76lhvkv0u5/7BHt8d8+QQ9Pe4zuvP8eQT7wxmuvQFouXs9bvmVZPooYmsuzlwmmBx5/VEF5cA+9TLfy5+l/cxOMFORrr87Wpj76EM2Z8zL5LZjLGoWl3H8RvTj9eS10+SJ0TGyXbzKdtAQDs7nCFCZSe/DxR1UmVRAI6Zzx8l+U9LgwimXp/dkjD6m3GiocuPO+qdri+f+ABJ+63rm2hvIshRTPnDVd5XsIrYJ7guwfP/ygxiSi1wr31BbNe1Nl6dbwO5eFvv/AvQJMusplhpRv0IfITspNVuinGNlIF8aH+tOyRW+T38K5FLZiMSFHzPYOr3DV5sz5m/qDB+/tU+Oo2KeffVq1RgfjnCZ5ndEoz7f5elvZLNCr/ztTnfa7J13/88TjvU8+/RuFG5O+tkYpn9OGe3MZFJRFLxcTzGV9WT8vTI5kmTJpN1mpn2XM4xa/kSXjKMe/x3m/Ww4SxzDTdkjyskVvabNnz+idOeulHjhflqggSAlWqPn1aHiqBil+iAnshKTCicMQItyfP7tkf/PX4B3z526UAWVBmdhf6EUZca5cUPRb5tvCp/SbJP4ZOUntW3Jy9A2GaS8lTPppBgawKby/kOWewPXUaA5kM5D8r3LcVgzfJ433sfIt4p8yOGZPQZJFWdxlAk7z98WyzObr4D2o8I/JV7M/IUnF5OnPebMSDfd+n65q9XsZfvucHMj3lCzb3XrWyiTX3MzN3MzN3Eaw/R9F0LfsfQgRsQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAxOC0wOC0wN1QxMzoxNToxNSswMDowMK6SKjoAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMTgtMDgtMDdUMTM6MTU6MTUrMDA6MDDfz5KGAAAAAElFTkSuQmCC

## [Bioinformatics](https://github.com/nothingelsematters/bioinformatics)

![Rust]

- **Semester**: 11
- **Content**: bioinformatics [Rosalind](https://rosalind.info/) problem solutions in **Rust**
- **Course structure**:
  - [Find the Longest Repeat in a String](https://github.com/nothingelsematters/bioinformatics/tree/main/src/longest_repeat.rs)
  - [Align Two Strings Using Affine Gap Penalties](https://github.com/nothingelsematters/bioinformatics/tree/main/src/affine_gap_align.rs)
  - [Reconstruct a String from its Paired Composition](https://github.com/nothingelsematters/bioinformatics/tree/main/src/paired_composition_reconstruction.rs)
  - [Find a Median String](https://github.com/nothingelsematters/bioinformatics/tree/main/src/median_string.rs)
  - [Implement the Viterbi Algorithm](https://github.com/nothingelsematters/bioinformatics/tree/main/src/viterbi.rs)

## [Parallel Algorithms](https://github.com/nothingelsematters/parallel-algorithms)

![Rust]

- **Semester**: 11
- **Course by**: Vitaly Aksenov
- **Content**: parallel algorithms benchmarking assignments in **Rust**
- **Course structure**:
  - [Parallel Quick Sort](https://github.com/nothingelsematters/parallel-algorithms/tree/main/src/quick_sort)
  - [Parallel Breadth First Search](https://github.com/nothingelsematters/parallel-algorithms/tree/main/src/bfs)

## [Big Data Machine Learning](big-data-machine-learning)

![Scala]

- **Semester**: 10
- **Content**: assignments in **Scala** using **[Spark](https://spark.apache.org/)**
- **Course structure**:
  - [Linear Regression with Spark](https://github.com/nothingelsematters/spark-linear-regression)

## [Deep Learning](deep-learning)

![Python]

- **Semester**: 10
- **Course by**: Denis Stepanov
- **Content**: miscellaneous theory and practice tasks in **Python** in **Jupyter notebooks**
- **Course structure**:
  - Several Jupyter Notebook tasks: both practice and theory
  - [Neural Style Transfer](https://github.com/nothingelsematters/neural-style-transfer)
  - [Model Distillation](https://github.com/nothingelsematters/model-distillation)

## [Compilers](https://github.com/nothingelsematters/compilers-course)

![Lama]

- **Semester**: 10
- **Course by**: Computer Science Center
- **Content**: writing a Lama compiler in a [Lama language](https://github.com/JetBrains-Research/Lama)
- **Course structure**:
  - [Expression interpretator](https://github.com/nothingelsematters/compilers-course/tree/A01-straight-line-int-sm)
  - [X86 assembler compiler](https://github.com/nothingelsematters/compilers-course/tree/A02-straight-line-x86)
  - [AST parser](https://github.com/nothingelsematters/compilers-course/tree/A03-straight-line-parser)
  - [Control Flow](https://github.com/nothingelsematters/compilers-course/tree/A04-control-flow)
  - [All expressions](https://github.com/nothingelsematters/compilers-course/tree/A05-all-expressions)
  - [Scope functions](https://github.com/nothingelsematters/compilers-course/tree/A06-scopes-functions)
  - [Data Structures](https://github.com/nothingelsematters/compilers-course/tree/A07-data-structures)
  - [Fixnum](https://github.com/nothingelsematters/compilers-course/tree/A08-fixnum)

## [Mathematical Models](mathematical-models)

![Python]

- **Semester**: 10
- **Content**: Several small random tasks
- **Course structure**:
  - [Dataset Visualization](mathematical-models/dataset-visualization.ipynb)

## [Data Processing and Analysis](data-processing)

![Python]

- **Semester**: 9
- **Course by**: Yang Qi
- **Content**: miscellaneous helpful data manipulations and transformations and a project
- **Course structure**:
  - [Sentiment Analysis of Microblog Data Streams](data-processing/sentiment)

## [Information Theory](https://github.com/nothingelsematters/information-theory/)

![Rust]

- **Semester**: 9
- **Course by**: Evgeny Belyaev
- **Content**: compressing algorithm implementations in **Rust**
- **Course structure**:
  - [Burrows Wheeler transformation with Huffman compression](https://github.com/nothingelsematters/information-theory/tree/main/burrows-wheeler)
  - [Jpeg format compression improver](https://github.com/nothingelsematters/information-theory/tree/main/jpg-improver) using BWT + MTF + RLE + AC

## [Optimization Methods](https://github.com/pool-party/optimization-methods/)

![Python]

- **Semester**: 8
- **Course by**: Mary Moskalenko
- **Content**: optimization method algorithm implementation and researching in **Python** and **Jupyter Notebooks**
- **Course structure**:
  - [First oder optimization methods](https://github.com/pool-party/optimization-methods/tree/master/first-order)

## [Software Design](software-design)

![Kotlin]

- **Semester**: 7-8
- **Course by**: A. Kirakozov
- **Content**: **Kotlin** languages homeworks solutions: different programming patterns and techniques
- **Course structure**:
  - [Assertions](software-design/assertions): LRU Cache implementation
  - [Modular and Mock object tests](software-design/api): Using VK API for requests
  - [Refactoring](software-design/refactoring)
  - [Model View Controller pattern](software-design/model-view-controller)
  - [Bridge pattern](software-design/bridge): Graph drawing desktop application
  - [Visitor pattern](software-design/visitor): Tokenizing, parsing, printing
  - [Aspect Oriented Programming paradigm](software-design/aspects): Aggregating code statistics
  - [Actors](software-design/actors): Different search engines request aggregation
  - [Reactive Application Example](software-design/reactive): Product shop backend application
  - [Event Sourcing](software-design/event-sourcing): Fitness center application

## [Software Testing](https://github.com/nothingelsematters/software-testing/)

![Kotlin]

- **Semester**: 7
- **Course by**: Vsevolod Brekelov
- **Content**: Miscellaneous patterns and instruments of testing, automatization and deployment
- **Course structure**:
  - [React framework](https://reactjs.org/) frontend with [node.js](https://nodejs.org/) backend testing:
    unit and component tests
  - [Lighthouse](https://developers.google.com/web/tools/lighthouse/)
    Accessibility, Best Practices, SEO: 100%
  - [Kotlin](https://kotlinlang.org/) + [Spring](https://spring.io/) + [PostgreqSQL](https://www.postgresql.org/)
    backend application testing:
    unit, component tests, mocking with [MockK](https://mockk.io/),
    [TestContainers](https://testcontainers.org/) database tests,
    [Allure report](https://docs.qameta.io/allure/) integration
  - E2E testing using miscellaneous frameworks: [Cypress](https://www.cypress.io/) in JavaScript,
    [Playwright](https://playwright.dev/) in TypeScript, [Selenide](https://selenide.org/) in Kotlin with
    [Selenoid](https://aerokube.com/selenoid/latest/) and [Allure report](https://docs.qameta.io/allure/) integrations
  - [Github Actions](https://github.com/features/actions) for Continious Integration
    (linting and testing) and Continious Deployment using [Heroku](https://heroku.com/)
  - Workshop Performance Testing using thing [Gatling](https://gatling.io/) Scala DSL:
    [Tinkoff Perf Workshop](https://gitlab.com/tinkoffperfworkshop/)

## [Network](network)

![Shell]

- **Semester**: 7
- **Course by**: Artem Beresnev
- **Content**: network connected laboratory reports
- **Course structure**:
  - [Console network components settings utilities in Linux and Windows OS](network/1-network-settings)

## [Machine Learning](machine-learning)

![C++]
![Python]

- **Semester**: 7
- **Course by**: Huawei
- **Content**: different maching learning algorithm implementation using **C++** and **Python**
- **Course structure**:
  - [kNN classifying method](machine-learning/kNN)
  - [linear regression using stochastic gradient descent with momentum and matrix method](machine-learning/linear-regression)
  - [Support Vector Machine](machine-learning/support-vector-machine)
  - [Naive Bayes Classifier](machine-learning/naive-bayes-classifier): spam and legit messages determining
  - [Decision Tree](machine-learning/decision-tree)
  - [Boost](machine-learning/boost)
  - [Clust](machine-learning/clust)
  > [different codeforces tasks](machine-learning/small-tasks)

## [Databases](databases)

![SQL]

- **Semester**: 7
- **Course by**: Georgiy Korneev
- **Content**: Homeworks creating University database step by step and a course project
- **Course structure**:
  - [Introduction: setting environment](databases/1-introduction)
  - [Models: ERM, PDM, DDL, DML](databases/2-model)
  - [Functional dependencies, keys, attribute set closure](databases/3-functional-dependencies)
  - [Normalization](databases/4-normalization)
  - [Relational Algebra](databases/5-relational-algebra)
  - [Relational Calculus, Datalog](databases/6-relational-calculus)
  - [Modification requests](databases/7-modification)
  > [Course project GitHub database](databases/project): 5 normal form normalization, ERM, PDM, DDL, DML, views,
  > functions, triggers

## [Numerical Methods](https://github.com/pool-party/numerical-methods/)

![C++]
![Python]

- **Semester**: 6
- **Course by**: Alex Segal
- **Content**: Homeworks and Course work

## [Distributed Systems](distributed-systems)

![Kotlin]

- **Semester**: 6
- **Course by**: Roman Elizarov
- **Content**: distributed systems homework in **Kotlin**
- **Course structure**:
  - [distributed mutex](distributed-systems/distributed-mutex)

## [Functional Programming](functional-programming)

![Haskell]

- **Semester**: 6
- **Course by**: Arseniy Seroka
- **Content**: **haskell** homeworks
- **Course structure**:
  - Practices:
    - [Sorting functions](functional-programming/practice1)
    - [Number operations](functional-programming/practice2)
  - Homeworks:

    1. [Theoretical](functional-programming/hw0)
    2. [Basic language constructions](functional-programming/hw1)
    3. [File System Shell](functional-programming/hw2)
    4. [Strictness. Multithreading. Advanced types. Lenses. Comonads.](functional-programming/hw3)

## [Probability Theory](probability-theory)

![Octave]

- **Semester**: 5-6
- **Course by**: Irina Suslina
- **Content**: probability algorithm implementations in **Octave**
- **Course structure**:
  - [Monte Carlo Method](probability-theory/monte-carlo)

## [Parallel Programming](parallel-programming)

![Java]
![Kotlin]

- **Semester**: 5
- **Course by**: Roman Elizarov, Nikita Koval
- **Content**: parallelism homeworks in **Java** and **Kotlin**
- **Course structure**:
  - [stack with elimination](parallel-programming/stack-elimination)
  - [Michael Scott queue](parallel-programming/msqueue)
  - [fine grained bank](parallel-programming/fine-grained-bank)
  - [linked list set](parallel-programming/linked-list-set)
  - [parallel dijkstra](parallel-programming/dijkstra)
  - [monotonic clock](parallel-programming/monotonic-clock)
  - [universal construction](parallel-programming/universal-construction)
  - [mcs lock](parallel-programming/mcs-lock)
  - [fetch-and-add queue](parallel-programming/faa-queue)
  - [synchronous queue](parallel-programming/synchronous-queue)
  - [lock free bank](parallel-programming/lock-free-bank)
  - [stm bank](parallel-programming/stm-bank)
  - [blocking stack](parallel-programming/blocking-stack)

## [Type Theory](type-theory)

![Haskell]

- **Semester**: 5
- **Course by**: Dmitry Shtukenberg
- **Content**: laboratory works in **Haskell** with make builds, alex and happy
- **Course structure**:
  - Lambda expression parser
    - [haskell version](type-theory/1-lambda-expression-parsing-haskell)
    - [ocaml version](type-theory/1-lambda-expression-parsing-ocaml)
  - [normalization](type-theory/2-normalization)
  - [expression type reducing](type-theory/3-type-deduction)

## [Cryptography](cryptography)

![Kotlin]

- **Semester**: 5
- **Course by**: Alla Levina
- **Content**: symmetric, stream, assymmetric ciphers and cryptographic hash functions implementation mostly in **Kotlin**
- **Course structure**:
  - Symmetric Ciphers
    - [Kasiski examination](kasiski-examination): hacking Vigenere cipher
    - [des cipher](des) (Data Encryption Standard)
    - [serpent cipher](serpent): Advanced Encryption Standard contest second place
  - Stream Ciphers
    - [rc4](rc4): simpliest stream cipher
    - [a5 and comp128](comp128): implementation of the A3, A5 and A8 functions defined in the GSM standard
    > A3 is used to authenticate the mobile station to the network. A8 is used to generate the session key used by A5 to encrypt the data transmitted between the mobile station and the BTS
  - Assymmetric Ciphers
    - [rsa](rsa): simpliest modulo operation based assymmetric cipher, one of the first public-key cryptosystems and is widely used for secure data transmission
  - Cryptographic Hash Functions
    - [CubeHash](cubehash): a cryptographic hash function submitted to the NIST hash function competition, SHA-3 semi-finalist

## [Translation Methods](translation-methods)

![Kotlin]

- **Semester**: 5
- **Course by**: Andrey Stankevich
- **Content**: regex laboratory work in **Perl**, parsers in **Kotlin**
- **Course structure**:
  - [Regular Expressions in Perl](translation-methods/regular-expressions)
  - [Manual building top-to-bottom syntax analyzers: c function headers](translation-methods/c-function-header)
  - [Using automatic analyzers generators Bison or ANTLR: functional to imperative language translation](translation-methods/functional2imperative)
  - [LALR parser generator](https://github.com/nothingelsematters/lalr-generator)

## [Operation Systems](operation-systems)

![C++]

- **Semester**: 4
- **Course by**: Dmitry Banschikov
- **Content**: homeworks in **C++** with make builds
- **Course structure**:
  - [Interpreter](operation-systems/terminal)
  - [Find utility subset](operation-systems/find)
  - [Piece of JIT complier](operation-systems/pseudo-jit)
  - [Introduction to libraries](operation-systems/libs-acquaintance)
  - [Introduction to sockets](operation-systems/synchronous-spcket-service)
  - [Introduction to descriptors transferring and IPC](operation-systems/net-descriptor-passing)
  - [Signals handling](operation-systems/sigsegv-handler)

## [Mathematical Logic](mathematical-logic)

![Haskell]

- **Semester**: 4
- **Course by**: Dmitry Shtukenberg
- **Content**: laboratory works in **Haskell** with make builds, alex and happy
- **Course structure**:
  - [expression parser](mathematical-logic/1-expression-parser)
  - [proof minimization](mathematical-logic/2-proof-minimization)
  - [intuitionistic proof conversion](mathematical-logic/3-intuitionistic-proof-conversion)
  - [propositional calculus completeness](mathematical-logic/4-propositional-calculus-completeness)
  - [formal arithmetic proof check](mathematical-logic/5-formal-arithmetic-proof-check)

## [Descrete Mathematics](discrete-maths)

![C++]

- **Semesters**: 1-4
- **Course by**: Andrey Stankevich, Artem Vasilyev
- **Content**: laboratory works mostly in **C++**
- **Course structure**:
  - [Probability](descrete-maths/probability)
  - [Language Theory and Automats](descrete-maths/languages)
  - Context Free Grammars
  - [Hamilton Paths](descrete-maths/hamilton-path)
  - [Graph Planarity](descrete-maths/graph-planarity)
  - [Generating Function](descrete-maths/generating-function)
  - [Turing machine](descrete-maths/turing-machine)

## [Algorithms and Data Structures](algorithms-and-data-structures)

![C++]

- **Semesters**: 1-4
- **Course by**: Pavel Mavrin
- **Content**: laboratory works mostly in **C++**
- **Course structure**:
  - [Dynamic Programming](algorithms-and-data-structures/dynamic-programming)
  - [Segment Tree](algorithms-and-data-structures/egment-tree)
  - [Binary Search Tree](algorithms-and-data-structures/binary-search-tree)
  - [Tree Algorithms: LCA, Link-Cut, etc](algorithms-and-data-structures/tree-algorithms)
  - [Greed Algorithms](algorithms-and-data-structures/greed)
  - [Graphs](algorithms-and-data-structures/graphs)
  - [Minimum Paths](algorithms-and-data-structures/minimum-path)
  - [Strings](algorithms-and-data-structures/strings)
  - [Maximum Flow And Matching](algorithms-and-data-structures/flow-and-matching)
  - [Maximum Flow Minimum Cost](algorithms-and-data-structures/maximum-flow-minimum-cost)
  - [Mathematic](algorithms-and-data-structures/mathematic)

## [Java](java)

![Java]

- **Semester**: 3
- **Course by**: Georgiy Korneev
- **Content**: homeworks in **Java** with buildscripts and javadoc
- **Course structure**:
  - [recursive walk](java/1-recursive-walk)
  - [array set](java/2-array-set)
  - [student db](java/3-student-db)
  - [implementor](java/4-implementor)
  - [jar implementor](java/5-jarimplementor)
  - [javadoc](java/6-javadoc)
  - [iterative parallelism](java/7-iterative-parallelism)
  - [parallel mapper](java/8-parallel-mapper)
  - [web crawler](java/9-web-crawler)
  - [hello udp](java/10-hello-udp)
  - [private persons](java/11-private-persons)

## [Web Programming](web)

![Java]

- **Semester**: 3
- **Course by**: Mike Mirzayanov
- **Content**: codeforces site implementation homeworks in **Java** with **Tomcat** ans **Spring**
- **Course structure**:
  - [HTTP (cURL usage, HTTP-requests, simple HTTP server)](web/1-server)
  - [Верстка (HTML + CSS)](web/2-front)
  - [Servlet API (Tomcat, JSON, CaptchaFilter)](web/3-servlets)
  - [Servlet API 2 (Java reflection, file database, Freemaker)](web/4-login)
  - [SQL (SQL basics, refactoring with Java reflection, MariaDB)](web/5-webmail)
  - [AJAX (Javascript, AJAX)](web/6-js)
  - [Spring (Spring Boot)](web/7-spring)
  - [Spring (OneToMany, ManyToOne, ManyToMany relations)](web/8-table-relations)
  - [Vue.js (Basics)](web/9-vue-js)
  - [Spring Rest API w/ Vue.js frontend](web/10-spring-api)

## [C++](c++)

![C++]

- **Semesters**: 2-3
- **Course by**: Ivan Sorokin
- **Content**: **C++** homework projects
- **Course structure**:
  - [Similar Files Finder: An utility to find files with similar content in directories](https://github.com/nothingelsematters/similar-files)
  - [Substring Finder: An utility to find the given substring in directories](https://github.com/nothingelsematters/substring-finder)
  - [Function: `std::function` implementation](https://github.com/nothingelsematters/function)

## [Paradigms of Programming](paradigms-of-programming)

![Java]
![JavaScript]

- **Semester**: 2
- **Course by**: Georgiy Korneev, Nikolay Vedernikov
- **Content**: homeworks in **Java** with some contracts and single **JavaScript** parser
- **Course structure**:
  - [calc sha256](paradigms-of-programming/1-calc-sah256)
  - [binary search](paradigms-of-programming/2-binary-search)
  - [array queue](paradigms-of-programming/3-array-queue)
  - [queue](paradigms-of-programming/4-queue)
  - [evaluate](paradigms-of-programming/5-evaluate)
  - [expression parser](paradigms-of-programming/6-expression-parser)
  - [exceptions](paradigms-of-programming/7-exceptions)
  - [functional expression](paradigms-of-programming/9-functional-expression)
  - [object expression](paradigms-of-programming/10-object-expression)
