package spread.sheet.w2o.processor.listener;

import spread.sheet.model.core.Row;
import spread.sheet.model.meta.SheetMeta;

/**
 * listener of row to object processor
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public interface RowProcessorListener<T> {

  /**
   * before object value set
   *
   * @param row       row
   * @param sheetMeta sheet meta
   * @param object    value not set
   */
  void before(Row row, SheetMeta sheetMeta, T object);

  /**
   * after object value set
   *
   * @param row       row
   * @param sheetMeta sheet meta
   * @param object    value set but same object
   */
  void after(Row row, SheetMeta sheetMeta, T object);
}
