package me.excel.tools.model.excel;

import me.excel.tools.model.template.ExcelSheetHeaderInfo;
import org.apache.poi.ss.usermodel.Sheet;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelSheetBean implements ExcelSheet {

  private int index;

  private String sheetName;

  private ExcelSheetHeaderInfo headerInfo = ExcelSheetHeaderInfo.SINGLE_SHEET_DEFAULT;

  private List<ExcelRow> excelRows = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  public ExcelSheetBean() {
    // default constructor
  }

  public ExcelSheetBean(String sheetName) {
    this.sheetName = sheetName;
  }

  public ExcelSheetBean(Sheet sheet) {
    this.sheetName = sheet.getSheetName();
  }

  @Override
  public ExcelSheetHeaderInfo getHeaderInfo() {
    return headerInfo;
  }

  @Override
  public void setHeaderInfo(ExcelSheetHeaderInfo headerInfo) {
    this.headerInfo = headerInfo;
  }

  @Override
  public int getIndex() {
    return index;
  }

  void setIndex(int index) {
    this.index = index;
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
    if (index < 1) {
      throw new IllegalArgumentException("index must greater than zero");
    }
    return excelRows.get(index - 1);
  }

  @Override
  public List<ExcelRow> getDataRows() {
    List<ExcelRow> dataRows = new ArrayList<>();
    for (int i = headerInfo.getDataStartAt(); i <= sizeOfRows(); i++) {
      dataRows.add(getRow(i));
    }
    return dataRows;
  }

  @Override
  public boolean addRow(ExcelRow excelRow) {
    ((ExcelRowBean) excelRow).setSheet(this);
    return excelRows.add(excelRow);
  }

  @Override
  public ExcelRow getFirstRow() {
    if (sizeOfRows() == 0) {
      return null;
    }
    return getRow(1);
  }

  @Override
  public ExcelRow getLastRow() {
    if (sizeOfRows() == 0) {
      return null;
    }
    return getRow(sizeOfRows());
  }

  public void setWorkbook(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public ExcelWorkbook getWorkbook() {
    return excelWorkbook;
  }

  @Override
  public Set<String> getDistinctValuesOfField(String field) {
    Set<String> distinctValues = new HashSet<>();

    for (ExcelRow excelRow : getDataRows()) {
      distinctValues.add(excelRow.getCell(field).getValue());
    }

    return distinctValues;
  }
}
