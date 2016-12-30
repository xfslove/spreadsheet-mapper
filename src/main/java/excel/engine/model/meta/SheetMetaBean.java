package excel.engine.model.meta;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetMetaBean implements SheetMeta {

  private String name;

  private int dataStartRowIndex;

  private List<FieldMeta> fieldMetas = new ArrayList<>();

  public SheetMetaBean(int dataStartRowIndex) {
    this.dataStartRowIndex = dataStartRowIndex;
  }

  public SheetMetaBean(String name, int dataStartRowIndex) {
    this.name = name;
    this.dataStartRowIndex = dataStartRowIndex;
  }

  @Override
  public String getName() {
    return name;
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
