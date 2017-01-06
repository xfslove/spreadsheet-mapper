package spreadsheet.mapper.w2o.processor.listener;

import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;

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
