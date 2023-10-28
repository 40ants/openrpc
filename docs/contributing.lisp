(uiop:define-package #:openrpc-docs/contributing
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:openrpc-docs/contributing)


(defsection @ask (:title "Our Ask...")
  "If you use this or find value in it, please consider contributing in one or more of the following ways:

   1. Click the \"Sponsor\" button at the top of the page and make a contribution.
   1. Star it!
   1. Share posts about it in social networks!
   1. Fix an issue.
   1. Add a feature (post a proposal in an issue first!).")


(defsection @contributors (:title "Contributors")
  "These people are contributed to OpenRPC. I'm so grateful to them!

   - [Kilian M. Haemmerle](https://github.com/kilianmh)")
