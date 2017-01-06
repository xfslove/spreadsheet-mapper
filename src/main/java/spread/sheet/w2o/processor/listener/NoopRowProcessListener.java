package spread.sheet.w2o.processor.listener;

import spread.sheet.model.core.Row;
import spread.sheet.model.meta.SheetMeta;

/**
 * Created by hanwen on 2017/1/3.
 */
public final class NoopRowProcessListener<T> implements RowProcessListener<T> {

  @Override
  public void before(Row row, SheetMeta sheetMeta, T object) {
    // nothing
  }

  @Override
  public void after(Row row, SheetMeta sheetMeta, T object) {
    // nothing
  }
}
