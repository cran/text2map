# text2map 0.1.3

## Improvements

* Add functions:
    - `find_transformation()` to norm, center, and align matrices
    - `find_projection()` finds the projection matrix onto a vector
    - `find_rejection()` finds the rejection matrix away from a vector
    - `dtm_melter()` quickly turns a DTM into a triplet dataframe (doc_id, term, count)
* Fixed `get_centroid()` naming (limits to single word for names)

# text2map 0.1.1

## Improvements

* Added functionality to `dtm_stopper()` to stop words by document or term frequencies
    * Nomenclature was changed, `stop_freq` was changed to `stop_termfreq`
* Added functionality to `dtm_resampler()` to resample proportion and fixed N lengths
* Added and clarified documentation
* Added a `NEWS.md` file to track changes to the package.
