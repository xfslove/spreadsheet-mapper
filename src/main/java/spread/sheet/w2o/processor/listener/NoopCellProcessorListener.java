package spread.sheet.w2o.processor.listener;

import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;

/**
 * Created by hanwen on 2017/1/3.
 */
public final class NoopCellProcessorListener<T> implements CellProcessorListener<T> {

  @Override
  public void before(Cell cell, FieldMeta fieldMeta, T object) {
    // nothing
  }

  @Override
  public void after(Cell cell, FieldMeta fieldMeta, T object) {
    // nothing
  }
}
