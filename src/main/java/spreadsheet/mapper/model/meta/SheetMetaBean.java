package spreadsheet.mapper.model.meta;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetMetaBean implements SheetMeta {

  private int sheetIndex = 1;

  private String sheetName;

  private int dataStartRowIndex;

  private List<FieldMeta> fieldMetas = new ArrayList<>();

  private WorkbookMeta workbookMeta;

  public SheetMetaBean(int dataStartRowIndex) {
    this(null, dataStartRowIndex);
  }

  public SheetMetaBean(String sheetName, int dataStartRowIndex) {
    this.sheetName = sheetName;
    this.dataStartRowIndex = dataStartRowIndex;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getSheetName() {
    return sheetName;
  }

  @Override
  public int getDataStartRowIndex() {
    return dataStartRowIndex;
  }

  @Override
  public List<FieldMeta> getFieldMetas() {
    Collections.sort(fieldMetas);
    return fieldMetas;
  }

  @Override
  public FieldMeta getFieldMeta(String fieldName) {
    for (FieldMeta fieldMeta : fieldMetas) {
      if (StringUtils.equals(fieldMeta.getName(), fieldName)) {
        return fieldMeta;
      }
    }
    return null;
  }

  @Override
  public boolean addFieldMeta(FieldMeta fieldMeta) {
    ((FieldMetaBean) fieldMeta).setSheetMeta(this);
    return fieldMetas.add(fieldMeta);
  }

  @Override
  public WorkbookMeta getWorkbookMeta() {
    return workbookMeta;
  }

  void setWorkbookMeta(WorkbookMeta workbookMeta) {
    this.workbookMeta = workbookMeta;
  }

  void setSheetIndex(int sheetIndex) {
    this.sheetIndex = sheetIndex;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("sheetName", sheetName)
        .append("dataStartRowIndex", dataStartRowIndex)
        .toString();
  }
}
