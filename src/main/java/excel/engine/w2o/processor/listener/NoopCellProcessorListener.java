package excel.engine.w2o.processor.listener;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

/**
 * Created by hanwen on 2017/1/3.
 */
public class NoopCellProcessorListener implements CellProcessorListener {

  @Override
  public void before(Cell cell, FieldMeta fieldMeta, Object object) {
    // nothing
  }

  @Override
  public void after(Cell cell, FieldMeta fieldMeta, Object object) {
    // nothing
  }
}
