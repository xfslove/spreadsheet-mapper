package spread.sheet.o2w.extractor;

import spread.sheet.model.meta.FieldMeta;

/**
 * value extractor
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public interface ValueExtractor<T> {

  /**
   * get human readable value to shown on cell
   *
   * @param object    supplied object
   * @param fieldMeta field meta
   * @return human readable value
   */
  String getStringValue(T object, FieldMeta fieldMeta);
}
