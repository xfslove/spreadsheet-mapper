package me.excel.tools;

import org.apache.commons.lang3.StringUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * model field extractor
 * <p>
 * Created by hanwen on 15-12-18.
 */
public class FieldUtils {

  private FieldUtils() {
  }

  public static final String BUSINESS_KEY_PREFIX = "businessKey.";

  /**
   * 获得business key的field
   *
   * @param field
   * @return
   */
  public static String getBusinessKeyField(String field) {
    if (!StringUtils.contains(field, BUSINESS_KEY_PREFIX)) {
      throw new IllegalStateException("field is not business key");
    }
    return field.substring(BUSINESS_KEY_PREFIX.length());
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
   * <p>
   * 获得Field, 查找算法是先从本类找, 如果找不到就递归到父类找, 如果找不到就返回null.
   * </p>
   * <p>
   * 支持private, private final, protected, protected final, public, public final 的field.
   * </p>
   *
   * @param clazz
   * @param fieldName
   * @return {@link Field}
   */
  public static Field getField(Class clazz, String fieldName) {
    try {
      return clazz.getDeclaredField(fieldName);
    } catch (NoSuchFieldException e) {
      if (clazz.getSuperclass() != null && !clazz.getSuperclass().equals(Object.class)) {
        return getField(clazz.getSuperclass(), fieldName);
      }
    }
    return null;
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
   * @param fieldName
   * @return
   */
  public static Class getFieldType(Class clazz, String fieldName) {

    Field field = getField(clazz, fieldName);

    if (field == null) {
      return null;
    }

    return field.getType();
  }
}
