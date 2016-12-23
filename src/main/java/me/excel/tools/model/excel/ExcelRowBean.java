package me.excel.tools.model.excel;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelRowBean implements ExcelRow {

  private int rowNum;

  private List<ExcelCell> excelCells = new ArrayList<>();

  private ExcelSheet excelSheet;

  public ExcelRowBean(int rowNum) {
    this.rowNum = rowNum;
  }

  public ExcelRowBean(Row row) {
    this.rowNum = row.getRowNum() + 1;
  }

  @Override
  public int getRowNum() {
    return rowNum;
  }

  @Override
  public List<ExcelCell> getCells() {
    return excelCells;
  }

  @Override
  public int sizeOfCells() {
    return excelCells.size();
  }

  @Override
  public ExcelCell getCell(int index) {
    if (index < 1) {
      throw new IllegalArgumentException("index must greater than zero");
    }
    return excelCells.get(index - 1);
  }

  @Override
  public boolean addCell(ExcelCell excelCell) {
    ((ExcelCellBean)excelCell).setRow(this);
    return excelCells.add(excelCell);
  }

  @Override
  public ExcelCell getFirstCell() {
    if (sizeOfCells() == 0) {
      return null;
    }
    return getCell(1);
  }

  @Override
  public ExcelCell getLastCell() {
    if (sizeOfCells() == 0) {
      return null;
    }
    return getCell(sizeOfCells());
  }

  @Override
  public ExcelCell getCell(String field) {
    for (ExcelCell excelCell : excelCells) {
      if (StringUtils.equals(excelCell.getField(), field)) {
        return excelCell;
      }
    }
    return null;
  }

  @Override
  public ExcelSheet getSheet() {
    return excelSheet;
  }

  public void setSheet(ExcelSheet excelSheet) {
    this.excelSheet = excelSheet;
  }

}
