package excel.engine.w2o.processor;

import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;

import java.util.List;

/**
 * do nothing listener
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public final class NoopSheetProcessorListener implements SheetProcessorListener {

  @Override
  public void beforeSheet(Sheet sheet) {
    // nothing
  }

  @Override
  public void beforeRow(Row row, Object object) {
    // nothing
  }

  @Override
  public void afterRow(Row row, Object object) {
    // nothing
  }

  @Override
  public void afterSheet(Sheet sheet, List<Object> objects) {
    // nothing
  }
}
