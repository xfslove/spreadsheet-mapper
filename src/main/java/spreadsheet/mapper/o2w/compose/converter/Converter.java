package spreadsheet.mapper.o2w.compose.converter;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * value converter
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public interface Converter<T> {

  /**
   * get human readable value to shown on cell
   *
   * @param object    supplied object
   * @param cell      convert which cell value
   * @param fieldMeta {@link FieldMeta}
   * @return human readable value
   */
  String getValue(T object, Cell cell, FieldMeta fieldMeta);
}
