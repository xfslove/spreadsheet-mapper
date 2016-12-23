package me.excel.tools.model.excel;

import me.excel.tools.FieldUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelSheetBean implements ExcelSheet {

  private String sheetName;

  private int index;

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

    for (int i = 0; i < excelRows.size(); i++) {
      if (i == 0 || i == 1 || i == 2) {
        continue;
      }
      dataRows.add(excelRows.get(i));
    }
    return dataRows;
  }

  @Override
  public int sizeOfData() {
    return getDataRows().size();
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

  @Override
  public Set<String> getDistinctCellValuesByField(String field) {

    if (StringUtils.isBlank(field)) {
      return Collections.emptySet();
    }

    Set<String> cellValuesOfField = new HashSet<>();

    for (ExcelRow excelRow : getDataRows()) {
      for (ExcelCell excelCell : excelRow.getCells()) {

        String f = excelCell.getField();
        String v = excelCell.getValue();

        if (FieldUtils.detectRealField(f).equals(FieldUtils.detectRealField(field)) && v != null) {
          cellValuesOfField.add(v);
        }
      }
    }
    return cellValuesOfField;
  }
}
