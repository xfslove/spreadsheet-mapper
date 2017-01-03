package excel.engine.w2o.processor.listener;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

/**
 * listener of cell value to field processor
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public interface CellProcessorListener {

  /**
   * before object value set
   *
   * @param cell      cell
   * @param fieldMeta field meta
   * @param object    value not set
   */
  void before(Cell cell, FieldMeta fieldMeta, Object object);

  /**
   * after object value set
   *
   * @param cell      cell
   * @param fieldMeta field meta
   * @param object    value set but same object
   */
  void after(Cell cell, FieldMeta fieldMeta, Object object);
}
