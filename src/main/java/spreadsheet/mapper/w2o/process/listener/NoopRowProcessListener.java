package spreadsheet.mapper.w2o.process.listener;

import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.SheetMeta;

/**
 * Created by hanwen on 2017/1/3.
 */
public final class NoopRowProcessListener<T> implements RowProcessListener<T> {

  @Override
  public void before(T object, Row row, SheetMeta sheetMeta) {
    // nothing
  }

  @Override
  public void after(T object, Row row, SheetMeta sheetMeta) {
    // nothing
  }
}
