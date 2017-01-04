package spread.sheet.model.core;

import org.apache.commons.lang3.builder.CompareToBuilder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 15-12-16.
 */
public class SheetBean implements Sheet {

  private int index;

  private String name;

  private List<Row> rows = new ArrayList<>();

  private Workbook workbook;

  public SheetBean(int index) {
    this.index = index;
  }

  public SheetBean(int index, String name) {
    this.index = index;
    this.name = name;
  }

  public SheetBean(org.apache.poi.ss.usermodel.Sheet sheet) {
    this.index = sheet.getWorkbook().getSheetIndex(sheet);
    this.name = sheet.getSheetName();
  }

  @Override
  public int getIndex() {
    return index;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public List<Row> getRows() {
    Collections.sort(rows);
    return rows;
  }

  @Override
  public int sizeOfRows() {
    return getRows().size();
  }

  @Override
  public Row getRow(int index) {
    if (index < 1 || index > sizeOfRows()) {
      throw new IllegalArgumentException("index out of bounds");
    }
    Collections.sort(rows);
    return rows.get(index - 1);
  }

  @Override
  public boolean addRow(Row row) {
    ((RowBean) row).setSheet(this);
    return rows.add(row);
  }

  @Override
  public Row getFirstRow() {
    if (sizeOfRows() == 0) {
      return null;
    }
    return getRow(1);
  }

  @Override
  public Row getLastRow() {
    if (sizeOfRows() == 0) {
      return null;
    }
    return getRow(sizeOfRows());
  }

  public void setWorkbook(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public Workbook getWorkbook() {
    return workbook;
  }

  @Override
  public int compareTo(Sheet o) {
    return new CompareToBuilder().append(index, o.getIndex()).toComparison();
  }
}
