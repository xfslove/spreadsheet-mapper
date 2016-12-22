package me.excel.tools.model.excel;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelWorkbookBean implements ExcelWorkbook {

  private final List<ExcelSheet> excelSheets = new ArrayList<>();

  public ExcelWorkbookBean() {
    // default constructor
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
    if (index < 1) {
      throw new IllegalArgumentException("index must greater than zero");
    }
    return excelSheets.get(index - 1);
  }

  @Override
  public ExcelSheet getLastSheet() {
    if (sizeOfSheets() == 0) {
      return null;
    }
    return getSheet(sizeOfSheets());
  }

  @Override
  public ExcelSheet getFirstSheet() {
    if (sizeOfSheets() == 0) {
      return null;
    }
    return getSheet(1);
  }
}
