package excel.engine.w2o.processor.listener;

import excel.engine.model.core.Row;
import excel.engine.model.meta.SheetMeta;

/**
 * Created by hanwen on 2017/1/3.
 */
public final class NoopRowProcessorListener implements RowProcessorListener {

  @Override
  public void before(Row row, SheetMeta sheetMeta, Object object) {
    // nothing
  }

  @Override
  public void after(Row row, SheetMeta sheetMeta, Object object) {
    // nothing
  }
}
