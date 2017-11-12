#!/bin/bash

for f in `find .. -name '*.re'` `find .. -name '*.rei'` ; \
do ( \
  if [[ $f =~ .*/test_data/.* ]] ; \
  then ( \
    refmt -w 90 --in-place $f; \
  ); else ( \
    refmt --in-place $f; \
  ); \
  fi \
); \
done

