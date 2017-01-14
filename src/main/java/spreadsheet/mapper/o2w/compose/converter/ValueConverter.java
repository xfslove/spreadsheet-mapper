package spreadsheet.mapper.o2w.compose.converter;

import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * value converter
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public interface ValueConverter<T> {

  /**
   * get human readable value to shown on cell
   *
   * @param object    supplied object
   * @param row       object shown on which row
   * @param fieldMeta {@link FieldMeta}
   * @return human readable value
   */
  String getStringValue(T object, Row row, FieldMeta fieldMeta);
}
