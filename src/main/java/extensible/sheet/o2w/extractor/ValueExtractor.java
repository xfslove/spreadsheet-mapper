package extensible.sheet.o2w.extractor;

import extensible.sheet.model.meta.FieldMeta;

/**
 * value extractor
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public interface ValueExtractor {

  /**
   * get human readable value to shown on cell
   *
   * @param data      supplied object
   * @param fieldMeta field meta
   * @return human readable value
   */
  String getStringValue(Object data, FieldMeta fieldMeta);
}
