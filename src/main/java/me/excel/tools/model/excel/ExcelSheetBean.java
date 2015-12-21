package me.excel.tools.model.excel;

import org.apache.poi.ss.usermodel.Sheet;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelSheetBean implements ExcelSheet {

  private String sheetName;

  private int index;

  private List<ExcelRow> excelRows = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  public ExcelSheetBean() {
  }

  public ExcelSheetBean(String sheetName) {
    this.sheetName = sheetName;
  }

  public ExcelSheetBean(Sheet sheet) {
    this.sheetName = sheet.getSheetName();
  }

  @Override
  public String getSheetName() {
    return sheetName;
  }

  @Override
  public List<ExcelRow> getRows() {
    return excelRows;
  }

  @Override
  public int sizeOfRows() {
    return excelRows.size();
  }

  @Override
  public ExcelRow getRow(int index) {
    return excelRows.get(index);
  }

  @Override
  public List<ExcelRow> getDataRows() {
    List<ExcelRow> dataRows = new ArrayList<>();

    for (int i = 0; i < dataRows.size(); i++) {
      if (i == 0 || i == 1 || i == 2) {
        continue;
      }
      dataRows.add(dataRows.get(i));
    }
    return dataRows;
  }

  @Override
  public boolean addRow(ExcelRow excelRow) {
    ((ExcelRowBean) excelRow).setSheet(this);
    return excelRows.add(excelRow);
  }

  @Override
  public ExcelRow getLastRow() {
    if (sizeOfRows() == 0) {
      return null;
    }
    return getRow(sizeOfRows() - 1);
  }

  public void setWorkbook(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
  }

  public ExcelWorkbook getWorkbook() {
    return excelWorkbook;
  }

  @Override
  public int getIndex() {
    return index;
  }

  public void setIndex(int index) {
    this.index = index;
  }

  @Override
  public boolean hasComments() {
    for (ExcelRow excelRow : excelRows) {
      for (ExcelCell excelCell : excelRow.getCells()) {
        if (excelCell.getComment() != null) {
          return true;
        }
      }
    }
    return false;
  }
}
