package spread.sheet.model.meta;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetMetaBean implements SheetMeta {

  private int sheetIndex;

  private String sheetName;

  private int dataStartRowIndex;

  private List<FieldMeta> fieldMetas = new ArrayList<>();

  public SheetMetaBean(int sheetIndex, int dataStartRowIndex) {
    this.sheetIndex = sheetIndex;
    this.dataStartRowIndex = dataStartRowIndex;
  }

  public SheetMetaBean(int sheetIndex, String sheetName, int dataStartRowIndex) {
    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
    this.dataStartRowIndex = dataStartRowIndex;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

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
  public FieldMeta getFieldMeta(String field) {
    for (FieldMeta fieldMeta : fieldMetas) {
      if (StringUtils.equals(fieldMeta.getName(), field)) {
        return fieldMeta;
      }
    }
    return null;
  }

  @Override
  public void addFieldMeta(FieldMeta fieldMeta) {
    ((FieldMetaBean) fieldMeta).setSheetMeta(this);
    this.fieldMetas.add(fieldMeta);
  }
}
