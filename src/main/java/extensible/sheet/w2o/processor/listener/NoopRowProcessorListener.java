package extensible.sheet.w2o.processor.listener;

import extensible.sheet.model.core.Row;
import extensible.sheet.model.meta.SheetMeta;

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
