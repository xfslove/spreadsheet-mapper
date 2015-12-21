package me.excel.tools.model.excel;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelWorkbookBean implements ExcelWorkbook {

  private final List<ExcelSheet> excelSheets = new ArrayList<>();

  public ExcelWorkbookBean() {
  }

  @Override
  public List<ExcelSheet> getSheets() {
    return excelSheets;
  }

  @Override
  public int sizeOfSheets() {
    return excelSheets.size();
  }

  @Override
  public boolean addSheet(ExcelSheet excelSheet) {
    boolean success = excelSheets.add(excelSheet);
    ((ExcelSheetBean) excelSheet).setIndex(sizeOfSheets());
    ((ExcelSheetBean) excelSheet).setWorkbook(this);
    return success;
  }

  @Override
  public ExcelSheet getSheet(int index) {
    return excelSheets.get(index);
  }

  @Override
  public ExcelSheet getLastSheet() {
    if (sizeOfSheets() == 0) {
      return null;
    }
    return getSheet(sizeOfSheets() - 1);
  }

}
