package me.excel.tools.processor;

import me.excel.tools.model.excel.Row;
import me.excel.tools.model.excel.Sheet;

import java.util.List;

/**
 * do nothing listener
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public final class NoopObjectProcessorListener implements ObjectProcessorListener {

  private int sheetIndex;

  public NoopObjectProcessorListener(int sheetIndex) {
    this.sheetIndex = sheetIndex;
  }

  @Override
  public void beforeSheet(Sheet sheet, List<Object> objects) {
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

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }
}
