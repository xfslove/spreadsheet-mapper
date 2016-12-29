package java.excel.engine.model.excel;

import java.excel.engine.model.ext.SheetTemplate;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by hanwen on 15-12-16.
 */
public class SheetBean implements Sheet {

  private int index;

  private String name;

  private SheetTemplate template;

  private List<Row> rows = new ArrayList<>();

  private Workbook workbook;

  public SheetBean() {
    // default constructor
  }

  public SheetBean(String name) {
    this.name = name;
  }

  public SheetBean(org.apache.poi.ss.usermodel.Sheet sheet) {
    this.name = sheet.getSheetName();
  }

  @Override
  public SheetTemplate getTemplate() {
    return template;
  }

  @Override
  public void setTemplate(SheetTemplate template) {
    this.template = template;
  }

  @Override
  public int getIndex() {
    return index;
  }

  void setIndex(int index) {
    this.index = index;
  }

  public String getName() {
    return name;
  }

  @Override
  public List<Row> getRows() {
    return rows;
  }

  @Override
  public int sizeOfRows() {
    return getRows().size();
  }

  @Override
  public Row getRow(int index) {
    if (index < 1) {
      throw new IllegalArgumentException("index must greater than zero");
    }
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
  public Row getFieldRow() {
    return rows.get(template.getFieldHeaderMeta().getRowIndex());
  }

  @Override
  public List<Row> getDataRows() {
    List<Row> dataRows = new ArrayList<>();
    for (int i = template.getDataStartRowIndex(); i <= sizeOfRows(); i++) {
      dataRows.add(getRow(i));
    }
    return dataRows;
  }

  @Override
  public int sizeOfDataRows() {
    return getDataRows().size();
  }

  @Override
  public Set<String> getDistinctValuesOfField(String field) {
    Set<String> distinctValues = new HashSet<>();

    for (Row row : getDataRows()) {
      distinctValues.add(row.getCell(field).getValue());
    }

    return distinctValues;
  }
}
