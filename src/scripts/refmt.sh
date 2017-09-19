for f in `find .. -name '*.re'` ; \
do ( \
refmt --in-place $f; \
); \
done

