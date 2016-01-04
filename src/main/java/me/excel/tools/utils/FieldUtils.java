package me.excel.tools.utils;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * model field utils
 *
 * Created by hanwen on 15-12-18.
 */
public class FieldUtils {

  private FieldUtils() {
  }

  /**
   * 获得去掉前缀的field name
   *
   * @param field 比如: model.name
   * @return 比如: name
   */
  public static String getFieldWithoutPrefix(String field) {
    List<String> splitFields = new ArrayList<>(Arrays.asList(field.split("\\.")));
    if (splitFields.size() == 1) {
      return field;
    } else {
      splitFields.remove(0);
      return StringUtils.join(splitFields, ".");
    }
  }

  /**
   * 获得field name对应的field type (支持 nested object)
   *
   * @param clazz
   * @param fields 比如: [objectB, objectA, name] 表示objectB.objectA.name
   * @return
   */
  public static Class getFieldType(Class clazz, String[] fields) {
    if (fields.length == 0 || clazz == null) {
      return null;
    }
    if (fields.length > 1) {
      String[] newFields = new String[fields.length - 1];
      for (int i = 0; i < newFields.length; i++) {
        newFields[i] = fields[i + 1];
      }
      return getFieldType(getFieldType(clazz, fields[0]), newFields);
    }
    return getFieldType(clazz, fields[0]);
  }

  /**
   * 获得field name对应的field type
   *
   * @param clazz
   * @param field
   * @return
   */
  public static Class getFieldType(Class<?> clazz, String field) {
    try {
      return clazz.getDeclaredField(field).getType();
    } catch (NoSuchFieldException e) {
      if (clazz.getSuperclass() != null || clazz.getSuperclass() != Object.class) {
        return getFieldType(clazz.getSuperclass(), field).getClass();
      }
    }
    return null;
  }
}
